# Supplementary Material

**"The Amplifier Effect of Artificial Agents in Social Contagion"**

This folder is the supplementary material accompanying the manuscript. It contains the full
code and data needed to reproduce every table and figure in the paper, comparing real human respondents to LLM agents in choice experiments, and simulating diffusion on mixed human-LLM networks.

The results must be reproduced sequentially, starting with the LLM prompt script (step 0) and proceeding to the final graphs (steps 1-9). Note that 1) the *.py file in step 0 requires access to the LLM API (via OpenRouter, see below) and therefore incurs API usage fees.
2) Then, run `0 install_packages.R` to install all R dependencies and follow steps 1-9, which involve only local computation. 
3) Running `fit_choice_model` and `run_diffusion` is computationally demanding.
Two conjoint experiments are used throughout the code (folder/variable name -> short label used
in plots and tables):

| `exp` value      | Short label | Topic                                            |
|------------------|-------------|---------------------------------------------------|
| `messaging_app`  | AA          | Adoption of a new messaging app                   |
| `energy_policy`  | PS          | Support for an energy policy                      |

## Folder layout

```
0 llm_*_prompt*.py                       step 0 -- generate AI "raw answers" via LLM queries
0 install_packages.R                     installs all R dependencies (run before steps 1-9)
1 fit_choice_model.R                     step 1
2 get_threshold_incentives_resistance.R  step 2
3 prepare_sample_data.R                  step 3
4 run_diffusion.R                        step 4
5 plot_main.R                            step 5 (main figures)
6 plot_SM.R                              step 6 (SM robustness-check figures)
7 get_tables.R                           network + demographics summary tables
8 plot_network_example.R                 illustrative network schematic figure
9 plot_pipeline_example.R                illustrative plots for study procedure

functions/            functions sourced
data/                 all input and intermediate/output data
pkg/                  bundled source of non-CRAN R packages (see Dependencies)
plots/                figures produced by scripts 5-9
tables/               LaTeX tables produced by script 7
```

## Data folder structure (per experiment, under `data/{messaging_app,energy_policy}/`)

| Folder             | Produced by | Contents                                                        |
|:--------------------|:------------|:-------------------------------------------------------------------|
| `raw answers/`      | step 0 (`0 llm_*.py`) for LLM agents; `*_real.csv` for human subjects | one file per human/LLM condition |
| `conjoint input/`    | (survey/study design) | Attribute/level definitions and product profile matrices used by the conjoint model |
| `coefs/`             | script 1    | HB utility coefficients |
| `thresholds/` `incentives/` `resistances/` | script 2 | adoption thresholds, incentives, resistance measures |
| `prepared_sample/`   | script 3    | Combined `.rda` sample objects (coefs + thresholds + incentives + resistances + profiles), one per condition |
| `diffusion/`         | script 4 | Network diffusion simulation results |
| `demographics/`      | prolific survey | Used by step 0 for persona-building and by script 7 for the demographics summary table |
| `data/network_addhealth/` | source: https://github.com/drguilbe/complexpaths | empirical AddHealth network CSVs (in `AddHealth_Networks_Largest_Components/`) and `sampled_nets.rda`, the fixed 18-network sample used by scripts 4, 7, 9. |



## Pipeline (run from scratch, in order)

Some R script has a `#exp=...` / `exp=...` pair near the top -- run it once per experiment by
toggling which line is active.

0. **`0 llm_{app,policy}_prompt{Baseline,Compressed,MatchedSet,NoMemory,ShuffledOrder}.py`** --
   Query an LLM (via OpenRouter) once per real respondent to answer the same conjoint choice
   tasks that respondent saw, producing the AI-condition `raw answers/` CSVs consumed by step 1.
   A dry-run mode (no `--key`) is available for testing the prompt construction
   without making API calls.
   **`0 install_packages.R`** -- Installs every R package listed under Dependencies below
   (CRAN packages, `diffuNet` from GitHub, and the bundled `conjointr` from `pkg/conjointr_0.1.0.tar.gz`). Run
   this once before steps 1-9.
1. **`1 fit_choice_model.R`** -- Fits a hierarchical Bayes choice model to each raw-answer file.
   Input: `data/{exp}/raw answers/*.csv`. Output: `data/{exp}/coefs/*.csv`.
2. **`2 get_threshold_incentives_resistance.R`** -- Computes adoption thresholds, incentives,
   and resistance from the fitted coefficients.
   Input: `data/{exp}/coefs/`, `data/{exp}/conjoint input/`.
   Output: `data/{exp}/thresholds|incentives|resistances/*.csv`.
3. **`3 prepare_sample_data.R`** -- Combines coefs/thresholds/incentives/resistances/profiles
   into one `.rda` sample object per condition.
   Input: outputs of steps 1-2. Output: `data/{exp}/prepared_sample/*.rda`. This step also drops
   respondents with negative social-signal utility: for `energy_policy` that removes 6 of 277
   real respondents (271 remain) but 0 of 277 for every AI condition; no respondents are dropped
   for `messaging_app` (284 in all conditions).
4. **`4 run_diffusion.R`** -- Simulates network diffusion for every combination of AddHealth
   network (18) x AI sample file (5) x AI-proportion (6) x seeding strategy
   (degree/low_th/neigh_susc/rand). 
   Input: `data/{exp}/prepared_sample/`, `data/network_addhealth/`.
   Output: `data/{exp}/diffusion/{exp}_diffusion_result.csv`.
5. **`5 plot_main.R`** -- Main figures (LLM agents with baseline prompt): variable importance,
   thresholds, adoption rate, partworth estimates, for both experiments.
   Input: `prepared_sample/`, diffusion result CSVs, `conjoint input/`.
   Output: `plots/*.png`, `plots/*.pdf`.
6. **`6 plot_SM.R`** -- Supplementary robustness-check figures comparing
   the baseline prompt against the 4 SM prompt variants.
   Input: `prepared_sample/` (main + `sm_*` files), `conjoint input/`.
   Output: `plots/*.png`, combined PDFs in `plots/`.
7. **`7 get_tables.R`** -- Two LaTeX summary tables: network statistics (18 AddHealth networks)
   and real-respondent demographics (N/% per category, per experiment).
   Input: `data/network_addhealth/`, `data/{exp}/demographics/*_real.csv`.
   Output: `tables/network_summary.tex`, `tables/demographics_summary.tex`.
8. **`8 plot_network_example.R`** -- Illustrative schematic figure (synthetic graph) showing human/LLM composition at three AI-proportion
   values. Input: NONE. Output: `plots/net illustration.png`.
9. **`9 plot_pipeline_example.R`** -- Create plot for illustrating study procedure.
   Step 1: simulates one diffusion example.
   Step 2: draws the threshold, network, and diffusion figures for this example.
   Input: prepared_sample .rda files, data/network_addhealth/.
   Output: `data/diffusion_example.csv`, plus `pipeline_example_threshold`,
   `pipeline_example_network_q00`/`q06`, and `pipeline_example_diffusion` in `plots/`.

## Dependencies

**R packages:** `data.table`, `dplyr`, `tidyr`, `foreach`, `doParallel`, `doMC`, `MASS`,
`DescTools`, `conjointr`, `ChoiceModelR`, `igraph`, `qgraph`, `diffuNet`, `ids`, `FinancialMath`,
`moments`, `ggplot2`, `ggridges`, `ggsignif`, `cowplot`, `patchwork`, `scales`, `grid`,
`gridExtra`, `latex2exp`. `conjointr` and `diffuNet` are not on CRAN. `diffuNet` is available at
`remotes::install_github("HiMifeng/diffuNet")`. `conjointr`'s source is bundled in this repo at
`pkg/conjointr_0.1.0.tar.gz` and is installed from there (no GitHub access needed); it is described in
Tănase, R., Algesheimer, R. & Mariani, M.S. Integrating behavioural experimental findings into
dynamical models to inform social change interventions. Nat Hum Behav 10, 873–883 (2026).
https://doi.org/10.1038/s41562-026-02417-4

Run **`0 install_packages.R`** once to install every R dependency listed above (CRAN packages,
`diffuNet`, and the bundled `conjointr`).

**Python packages:** `pandas`, `tqdm`, `openrouter` (only required for real, non-dry-run LLM
calls; stdlib otherwise: `argparse`, `random`, `re`, `time`, `itertools`, `pathlib`).
