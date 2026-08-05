"""
Description:
    Pairs each human respondent's demographic persona
    (`data/energy_policy/demographics/energypolicy_real.csv`) with an LLM agent that answers 14
    choice tasks.

    compressed prompt: uses a compressed background description (see build_background()) instead of
    the full explanatory text. each question's 3 alternatives are drawn independently at random from the
    full attribute combination space (see random_profiles()), NOT the choice sets the paired
    human respondent actually saw.

Usage:
    export OPENAI_API_KEY=sk-...
    python3 llm_policy_promptCompressed.py --limit 5      # smoke test
    python3 llm_policy_promptCompressed.py

Input: demographics from `data/energy_policy/demographics/energypolicy_real.csv`; choice sets are randomly generated
Output: `data/energy_policy/raw answers/sm_energypolicy_{model}_temp{temperature}_promptCompressed.csv`
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
study_dir = base_dir / "data" / "energy_policy"
demographics_csv = study_dir / "demographics" / "energypolicy_real.csv"
answers_csv = study_dir / "raw answers" / "energypolicy_real.csv"

VARIANT_NAME = "promptCompressed"


MODEL_LABEL_PATTERNS = [
    (r"claude", "claude"),
    (r"gpt-?5-mini", "gpt5"),
    (r"deepseek", "deepseek"),
]


def extract_model_label(model):
    """Extract a short canonical model label (gpt5/deepseek) from a full model identifier."""
    for pattern, label in MODEL_LABEL_PATTERNS:
        if re.search(pattern, model, re.IGNORECASE):
            return label
    raise ValueError(f"Could not determine a short model label for model={model!r}")


def default_output_csv(model, temperature, dry_run=False):
    model_label = extract_model_label(model)
    suffix = ".dryrun.csv" if dry_run else ".csv"
    return study_dir / "raw answers" / f"sm_energypolicy_{model_label}_temp{temperature:g}_{VARIANT_NAME}{suffix}"


N_QUESTIONS = 14
NONE_ALT = 4
MAX_API_RETRIES = 5
API_BACKOFF_BASE_SECONDS = 2
MAX_PARSE_RETRIES = 3
OUTPUT_COLUMNS = [
    "resp.id", "ques", "alt",
    "policytype", "cost", "year", "distance", "org", "adopters",
    "none", "choice",
]

# Define value mapping
VALUE_MAPPING = {
    "Age": {1: "18-24", 2: "25-34", 3: "35-44", 4: "45-54", 5: "55-64", 6: "65-74", 7: "75 or above"},
    "Gender": {1: "male", 2: "female"},
    "Education": {1: "Less than high school degree", 2: "High school graduate (high school diploma or equivalent including GED)", 3: "Some college but no degree", 4: "Associate degree in college (2-year)", 5: "Bachelor's degree in college (4-years)", 6: "Master's degree", 7: "Doctoral degree", 8: "Professional degree (JD, MD)", 9: "Prefer not to say"},
    "Subj": {1: "Economics", 2: "Humanities", 3: "Science", 4: "not Economics, Humanities or Science"},
    "Income": {1: "under $25,000", 2: "$25,001 - $49,999", 3: "$50,000 - $74,999", 4: "$75,000 - $99,999", 5: "$100,000 - $149,999", 6: "$150,000 - $249,999", 7: "more than $250,000"},
    "Politics": {1: "conservative and nationalist", 2: "liberal and anti-traditional", 3: "not conservative or liberal"},
}

# Same logic as the original script: 3 alternatives per question are drawn independently at
# random from the full attribute combination space, unrelated to what the paired human
# respondent actually saw (unlike llm_policy_promptMatchedSet.py, which replays their real choice sets).
ATTRIBUTE_LEVELS = {
    "policytype": ["ban", "subsidies", "tax"],
    "cost": [4, 9, 14, 19],
    "year": [2025, 2035, 2045, 2055],
    "distance": [2, 5, 10, 50],
    "org": ["ccc", "dp", "greenpeace", "rp"],
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


def build_persona(age_code, gender_code, education_code, subj_code, income_code, politics_code):
    """The respondent's demographic self-description (the part that varies per resp.id)."""
    age = VALUE_MAPPING["Age"].get(age_code, age_code)
    gender = VALUE_MAPPING["Gender"].get(gender_code, gender_code)
    education = VALUE_MAPPING["Education"].get(education_code, education_code)
    subj = VALUE_MAPPING["Subj"].get(subj_code, subj_code)
    income = VALUE_MAPPING["Income"].get(income_code, income_code)
    politics = VALUE_MAPPING["Politics"].get(politics_code, politics_code)

    return (
        f"You are a {age} years old {gender}, "
        f"your highest level of school you have completed or the highest degree you have received is {education}, "
        f"your major subject of study was {subj}, your total yearly household income before taxes is approximately {income}, "
        f"you describe your political orientation as {politics}.\n"
    )


def build_background():
    """Compressed variant: both the opening CCS scenario framing and the 6 aspect paragraphs
    from llm_policy_promptBaseline.py are shortened. Task instructions are left
    unchanged, so this isolates "background length" as the single manipulated variable."""
    return (
        "Carbon capture and storage (CCS) captures, transports, and permanently stores CO2 emitted by fossil-fuel power plants and industrial facilities, to help address climate change. "
        "Some see it as a promising climate solution; others see it as costly and risky, and there is ongoing political debate over how to regulate and implement it.\n"
        "You may or may not support scaling up CCS, but you may still have preferences among different ways it could be implemented in your state. Below are several scenarios for a CCS scale-up; please evaluate them.\n"
        "The below-mentioned policy scenarios each consist of 6 aspects:\n"
        "1. Policy type: a ban on new fossil fuel power plants without CCS, government subsidies for CCS, or a tax increase on fossil fuel power generation without CCS.\n"
        "2. Policy cost: between $4 and $19 per household per month.\n"
        "3. Beginning of policy implementation: 2025, 2035, 2045, or 2055.\n"
        "4. Distance of CCS facilities from residential areas: 2, 5, 10, or 50 miles.\n"
        "5. Policy endorsement: by stakeholders such as Greenpeace or the Carbon Capture Coalition (ccc), or political parties such as Democrats (dp) or Republicans (rp).\n"
        "6. Percentage of your friends who endorse the policy scenario: the share of your friends, out of your total number of friends, who endorse it.\n"
        "You will repeatedly see three different policy scenarios and I will ask you which one you would prefer. If you think you wouldn't prefer any, feel free to choose the None option."
    )


def build_question(alt_rows):
    """alt_rows: DataFrame with the 3 real alternatives (alt 1-3) for one (resp.id, ques)."""
    lines = []
    for _, row in alt_rows.sort_values("alt").iterrows():
        pct = int(round(float(row["adopters"]) * 100))
        lines.append(
            f"Option {int(row['alt'])} is a {row['policytype']} policy, costs ${row['cost']} per household per month, "
            f"will be implemented in {row['year']}, the required distance to residential areas is {row['distance']} miles, "
            f"is endorsed by {row['org']}, and {pct}% of your friends endorse it."
        )
    lines.append(
        f"Option {NONE_ALT} is to choose no policy. Which option do you choose? You have to pick one option. "
        "Don't explain your choice, just name the option you choose."
    )
    return " ".join(lines)


def call_llm(client, model, temperature, messages):
    """Calls the Chat Completions API with retry + exponential backoff on transient errors."""
    last_error = None
    for attempt in range(MAX_API_RETRIES):
        try:
            rs = client.chat.send(
                model=model, messages=messages, temperature=temperature,
            )
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
    persona = build_persona(
        demo_row["Age"], demo_row["Gender"], demo_row["Education"],
        demo_row["subj"], demo_row["Income"], demo_row["politcs"],
    )
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
                "policytype": alt_row["policytype"], "cost": alt_row["cost"], "year": alt_row["year"],
                "distance": alt_row["distance"], "org": alt_row["org"], "adopters": alt_row["adopters"],
                "none": 2,
                "choice": chosen if int(alt_row["alt"]) == 1 else 0,
            })
        rows.append({
            "resp.id": resp_id, "ques": ques, "alt": NONE_ALT,
            "policytype": "0", "cost": "0", "year": "0", "distance": "0", "org": "0", "adopters": "0",
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
    return pd.read_csv(demographics_csv).set_index("resp.id")[["Age", "Gender", "Education", "subj", "Income", "politcs"]]


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
