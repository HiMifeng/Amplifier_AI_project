"""
Description:
    Pairs each human respondent's demographic persona
    (`data/messaging_app/demographics/app_real.csv`) with an LLM agent that answers 14 choice tasks.

    Baseline: each question's 3 alternatives are drawn independently at random from the
    full attribute combination space (see random_profiles()), NOT the choice sets the paired
    human respondent actually saw.

Usage:
    export OPENAI_API_KEY=sk-...
    python3 llm_app_promptBaseline.py --limit 5      # smoke test
    python3 llm_app_promptBaseline.py

Input: demographics from `data/messaging_app/demographics/app_real.csv`; choice sets are randomly generated
Output: `data/messaging_app/raw answers/app_{model}_temp{temperature}.csv`
"""

import argparse
import random
import re
import time
from itertools import product
from pathlib import Path

import pandas as pd
# from openai import OpenAI
from tqdm import tqdm

try:
    from openrouter import OpenRouter
except ImportError:
    OpenRouter = None  # only required for a real (non-dry-run) run

base_dir = Path(__file__).resolve().parent
study_dir = base_dir / "data" / "messaging_app"
demographics_csv = study_dir / "demographics" / "app_real.csv"
answers_csv = study_dir / "raw answers" / "app_real.csv"


VARIANT_NAME = "promptBaseline"


MODEL_LABEL_PATTERNS = [
    (r"gemini", "gemini"),
    (r"claude", "claude"),
    (r"deepseek", "deepseek"),
    (r"gpt-?3\.5", "gpt3.5"),
    (r"gpt-?5-mini", "gpt5"),
]


def extract_model_label(model):
    #Extract a short model label (gemini/claude/deepseek/gpt3.5/gpt5) from a full model identifier
    for pattern, label in MODEL_LABEL_PATTERNS:
        if re.search(pattern, model, re.IGNORECASE):
            return label
    raise ValueError(f"Could not determine a short model label for model={model!r}")


def default_output_csv(model, temperature, dry_run=False):
    model_label = extract_model_label(model)
    suffix = ".dryrun.csv" if dry_run else ".csv"
    return study_dir / "raw answers" / f"app_{model_label}_temp{temperature:g}{suffix}"


N_QUESTIONS = 14
NONE_ALT = 4
MAX_API_RETRIES = 5
API_BACKOFF_BASE_SECONDS = 2
MAX_PARSE_RETRIES = 3
OUTPUT_COLUMNS = [
    "resp.id", "ques", "alt", 
    "accessibility", "authentication", "customisation", "video", "adopters",
    "none","choice", 
]

# Define value mapping
VALUE_MAPPING = {
    "Age": {1: "18-24", 2: "25-34", 3: "35-44", 4: "45-54", 5: "55-64", 6: "65-74", 7: "75 or above"},
    "Gender": {1: "male", 2: "female"},
    "Education": {1: "Less than high school degree", 2: "High school graduate (high school diploma or equivalent including GED)", 3: "Some college but no degree", 4: "Associate degree in college (2-year)", 5: "Bachelor's degree in college (4-years)", 6: "Master's degree", 7: "Doctoral degree", 8: "Professional degree (JD, MD)", 9: "Prefer not to say"},
    "Income": {1: "under $25,000", 2: "$25,001 - $49,999", 3: "$50,000 - $74,999", 4: "$75,000 - $99,999", 5: "$100,000 - $149,999", 6: "$150,000 - $249,999", 7: "more than $250,000"},
    "Subj": {1: "Economics", 2: "Humanities", 3: "Science", 4: "not Economics, Humanities or Science"},
}

ACCESSIBILITY_DESC = {"mobile": "mobile only", "web": "web accessible"}
AUTHENTICATION_DESC = {
    "simple": "simple authentication",
    "two_factor": "two-factor authentication",
    "multi_factor": "multi-factor authentication",
}
CUSTOMISATION_DESC = {"low": "low", "medium": "medium", "high": "high"}
VIDEO_DESC = {"one_to_one": "one-on-one", "multi_person": "multi-person"}

# Same logic as the original script: 3 alternatives per question are drawn independently at
# random from the full attribute combination space, unrelated to what the paired human
# respondent actually saw (unlike llm_app_promptMatchedSet.py, which replays their real choice sets).
ATTRIBUTE_LEVELS = {
    "accessibility": list(ACCESSIBILITY_DESC.keys()),
    "authentication": list(AUTHENTICATION_DESC.keys()),
    "customisation": list(CUSTOMISATION_DESC.keys()),
    "video": list(VIDEO_DESC.keys()),
    "adopters": [0.01, 0.23, 0.45, 0.76, 0.98],
}
ALL_PROFILES = pd.DataFrame(
    list(product(*ATTRIBUTE_LEVELS.values())), columns=ATTRIBUTE_LEVELS.keys()
)


def random_profiles():
    """3 freshly-sampled random alternatives, matching the original script's profiles.sample(n=3)."""
    sampled = ALL_PROFILES.sample(n=3).reset_index(drop=True)
    sampled["alt"] = [1, 2, 3]
    return sampled


def build_persona(age_code, gender_code, education_code, subj_code, income_code):
    """The respondent's demographic self-description (the part that varies per resp.id)."""
    age = VALUE_MAPPING["Age"].get(age_code, age_code)
    gender = VALUE_MAPPING["Gender"].get(gender_code, gender_code)
    education = VALUE_MAPPING["Education"].get(education_code, education_code)
    subj = VALUE_MAPPING["Subj"].get(subj_code, subj_code)
    income = VALUE_MAPPING["Income"].get(income_code, income_code)

    return (
        f"You are a {age} years old {gender}, your highest level of school you have completed or the highest degree you have received is {education}, "
        f"your major subject of study was {subj}, and your total household income during the past 12 months was {income}.\n"
    )


def build_background():
    """The study scenario, attribute definitions, and task instructions (identical for every respondent)."""
    return (
        "Imagine there are several new multiple instant messaging apps on the market. All apps are free and are similar to each other in all but the aspects described "
        "below. Furthermore, we ask you to imagine several of your friends are already using such an app. We will show you this information as one of the app attributes.\n"
        "The apps differ in terms of the following attributes.\n"
        "1. Accessibility: Instant messaging apps differ in the way you can access them. They can be:\n"
        "Mobile only: A mobile only app is specifically developed for smartphones and tablets. It takes full advantage of mobile device features such as push notifications, camera integration, and location services. It offers a "
        "seamless, on-the-go communication experience, but it's not accessible on desktop or web browsers.\n"
        "Web accessible: Web-accessible instant messaging apps expand their reach beyond mobile devices. They allow users to access their chats and conversations via web browsers on desktop computers or laptops. This "
        "versatility enables seamless transition between devices, convenient typing with a physical keyboard, and the ability to share files and links more easily on a larger screen.\n"
        "2. Authentication: Authentication is important to safeguard your personal information and ensure that your conversations remain private. The apps can use one of the three levels of authentication described below, sorted by the least to the most secure:\n"
        "Simple authentication: Login with username and password.\n"
        "Two-factor authentication: Two-factor authentication (2FA) requires an additional authentication method beyond your username and password. This involves receiving a one-time verification code via SMS or email, which you must enter alongside your password to access your account.\n"
        "Multi-factor authentication: In addition to your username, password, and the SMS or email verification code, you must also verify your identity using a fingerprint scanner or a hardware token (a device connected to your mobile or computer).\n"
        "3. Customisation level: The customization level determines how much you can personalize your messaging experience. It can take one of the following values:\n"
        "Low: You can adjust the basic settings, like security and notification preferences.\n"
        "Medium: In addition to the basic settings, you have the flexibility to shape your chat organization, such as creating chat lists and pinning important conversations to the top.\n"
        "High: Additionally, you have the option to customize themes and appearance, including elements like color schemes, backgrounds, fonts used and many others.\n"
        "4. Video calls: To make the most of your video communication experience, apps focus either on One-on-one or multi-person video calls.\n"
        "One-on-One: The app provides a straightforward and personal video calling experience designed and optimised for one-on-one interactions. The app does not support video calls between more than two people at once.\n"
        "Multi-person: The app offers a versatile video calling feature, allowing you to connect with multiple participants simultaneously.\n"
        "5. Percentage of friends already using the app: Imagine several of your friends use the app. This attribute shows the percentage of friends who already use this particular app. You can think about it as how many of your friends, out of all your friends are using the app. For example, if you have 10 friends and 5 of them already use the app, the percentage would be 50%.\n"
        "I will repeatedly show you 3 apps which differ in terms of the attributes previously described and ask you to select which one (out of "
        "the three) you would use instead of the app you are currently using. If you don't like any of the options, please feel free to select the None option."
    )


def build_question(alt_rows):
    """alt_rows: DataFrame with the 3 real alternatives (alt 1-3) for one (resp.id, ques)."""
    lines = []
    for _, row in alt_rows.sort_values("alt").iterrows():
        pct = int(round(float(row["adopters"]) * 100))
        lines.append(
            f"Option {int(row['alt'])} is {ACCESSIBILITY_DESC[row['accessibility']]}, "
            f"has {AUTHENTICATION_DESC[row['authentication']]}, a {CUSTOMISATION_DESC[row['customisation']]} "
            f"customisation level, and allows {VIDEO_DESC[row['video']]} calls and {pct}% of your friends are "
            "already using the app."
        )
    lines.append(
        f"Option {NONE_ALT} is to use no app. Which option do you choose? You have to pick one option. "
        "Don't explain your choice, just name the option you choose."
    )
    return " ".join(lines)


def call_llm(client, model, temperature, messages):
    """Calls the Chat Completions API with retry + exponential backoff on transient errors."""
    last_error = None
    for attempt in range(MAX_API_RETRIES):
        try:
            # return client.chat.completions.create(
            # print(messages)
            rs = client.chat.send(
                model=model, messages=messages, temperature=temperature,
            )
            # print(rs)
            # print("="*50)
            return rs
        except Exception as exc:
            last_error = exc
            wait = API_BACKOFF_BASE_SECONDS * (2 ** attempt)
            print(f"  [API error] {exc} — retrying in {wait}s (attempt {attempt + 1}/{MAX_API_RETRIES})")
            time.sleep(wait)
    raise RuntimeError(f"OpenAI API call failed after {MAX_API_RETRIES} attempts: {last_error}")


def parse_choice(text):
    match = re.search(r"\b([1-4])\b", text)
    return int(match.group(1)) if match else None


def ask_question(client, model, temperature, messages, question_text):
    """Appends the question, asks the LLM, retries once on unparseable replies, returns (choice, reply_text)."""
    messages.append({"role": "user", "content": question_text})

    chosen = None
    completion_text = ""
    for parse_attempt in range(MAX_PARSE_RETRIES):
        response = call_llm(client, model, temperature, messages)
        completion_text = response.choices[0].message.content or ""  # some models return content=None (e.g. refusal/empty generation)
        chosen = parse_choice(completion_text)
        if chosen is not None:
            break
        # silent retry, matching the original script: resend the same messages, don't grow the conversation

    messages.append({"role": "assistant", "content": completion_text})
    return chosen or 0  # 0 = unparseable after retries, treated as a missing response


def run_respondent(client, model, temperature, resp_id, demo_row, resp_choice_sets):
    # resp_choice_sets (the human respondent's real choice sets) is accepted for interface
    # consistency with the other variants' run_respondent(), but intentionally unused here:
    # each question's 3 alternatives are freshly randomized instead, see random_profiles().
    persona = build_persona(demo_row["Age"], demo_row["Gender"], demo_row["Education"], demo_row["subj"], demo_row["Income"])
    background = build_background()
    messages = [{"role": "user", "content": f"{persona} {background}"}]

    rows = []
    for ques in range(1, N_QUESTIONS + 1):
        alt_rows = random_profiles()

        question_text = build_question(alt_rows)
        chosen = ask_question(client, model, temperature, messages, question_text)

        for _, alt_row in alt_rows.sort_values("alt").iterrows():
            rows.append({
                "resp.id": resp_id, "ques": ques, "alt": int(alt_row["alt"]),
                "accessibility": alt_row["accessibility"], "authentication": alt_row["authentication"],
                "customisation": alt_row["customisation"], "video": alt_row["video"],
                "adopters": alt_row["adopters"],
                "none": 2,
                "choice": chosen if int(alt_row["alt"]) == 1 else 0,
            })
        rows.append({
            "resp.id": resp_id, "ques": ques, "alt": NONE_ALT,
            "accessibility": "0", "authentication": "0", "customisation": "0", "video": "0", "adopters": "0",
            "none": 1, "choice": 0,
        })

    return pd.DataFrame(rows, columns=OUTPUT_COLUMNS)


class _FakeMessage:
    def __init__(self, content):
        self.content = content


class _FakeChoice:
    def __init__(self, content):
        self.message = _FakeMessage(content)


class _FakeResponse:
    def __init__(self, content):
        self.choices = [_FakeChoice(content)]


class _FakeChat:
    def send(self, model, messages, temperature):
        # picks a plausible random option (1..NONE_ALT) instead of calling any real API
        return _FakeResponse(str(random.randint(1, NONE_ALT)))


class FakeClient:
    """Stands in for the real LLM client so the pipeline can be smoke-tested with --dry-run,
    without needing API access."""

    def __init__(self):
        self.chat = _FakeChat()


def load_demographics():
    return pd.read_csv(demographics_csv).set_index("resp.id")[["Age", "Gender", "Education", "subj", "Income"]]


def already_done_resp_ids(output_csv):
    if not output_csv.exists():
        return set()
    done = pd.read_csv(output_csv, usecols=["resp.id"])
    return set(done["resp.id"].unique())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model", default="gpt-3.5-turbo-0613")
    parser.add_argument("--key", default=None, type=str, help="replace the key with real key.")
    parser.add_argument("--temperature", type=float, default=1.0)
    parser.add_argument("--output", type=Path, default=None)
    parser.add_argument("--limit", type=int, default=None, help="only process the first N respondents (for testing)")
    parser.add_argument("--start-fresh", action="store_true", help="delete existing output and start over")
    parser.add_argument("--dry-run", action="store_true",
                         help="don't call any real API; use a fake client that returns random choices, "
                              "to smoke-test the pipeline (persona building, message threading, CSV output, "
                              "resumability) without API access")
    args = parser.parse_args()

    if args.output is None:
        args.output = default_output_csv(args.model, args.temperature, dry_run=args.dry_run)

    if args.start_fresh and args.output.exists():
        args.output.unlink()

    if args.dry_run:
        print("--dry-run: using a fake client, no real API calls will be made.")
        client = FakeClient()
    else:
        if OpenRouter is None:
            raise RuntimeError("the 'openrouter' package is not installed; use --dry-run to test without it")
        client = OpenRouter(api_key=args.key)

    demographics = load_demographics()
    answers = pd.read_csv(answers_csv)
    choice_sets = answers[answers["alt"] != NONE_ALT]

    resp_ids = list(dict.fromkeys(answers["resp.id"]))  # preserves file order, de-duplicated
    if args.limit:
        resp_ids = resp_ids[: args.limit]

    done_ids = already_done_resp_ids(args.output)
    todo_ids = [r for r in resp_ids if r not in done_ids]
    print(f"{len(done_ids)} respondents already done, {len(todo_ids)} remaining out of {len(resp_ids)} total.")

    header_needed = not args.output.exists()
    failed_ids = []
    for resp_id in tqdm(todo_ids, desc="Respondents"):
        demo_row = demographics.loc[resp_id]
        resp_choice_sets = choice_sets[choice_sets["resp.id"] == resp_id]

        try:
            resp_df = run_respondent(client, args.model, args.temperature, resp_id, demo_row, resp_choice_sets)
        except Exception as exc:
            print(f"  [skipped] resp.id={resp_id} failed: {exc}")
            failed_ids.append(resp_id)
            continue

        resp_df.to_csv(args.output, mode="a", header=header_needed, index=False)
        header_needed = False

    print(f"Done. Results saved to {args.output}")
    if failed_ids:
        print(f"{len(failed_ids)} respondents failed and were skipped (re-run the script to retry them): {failed_ids}")


if __name__ == "__main__":
    main()
