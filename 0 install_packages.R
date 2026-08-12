# Description: Install all R packages required by this pipeline (see README.md "Dependencies").
# Run once before running any of the numbered pipeline scripts (1-9).

cran_packages <- c(
  "data.table", "dplyr", "tidyr", "foreach", "doParallel", "doMC", "MASS",
  "DescTools", "ChoiceModelR", "igraph", "qgraph", "ids", "FinancialMath",
  "moments", "ggplot2", "ggridges", "ggsignif", "cowplot", "patchwork",
  "scales", "gridExtra", "latex2exp", "remotes"
)

new_cran <- setdiff(cran_packages, rownames(installed.packages()))
if (length(new_cran) > 0) install.packages(new_cran)

# conjointr and diffuNet are not on CRAN; 
# install from GitHub sources (installation is publicly available)
if (!requireNamespace("diffuNet", quietly = TRUE)) remotes::install_github("HiMifeng/diffuNet")

# The `conjointr` package is described in
# Tănase, R., Algesheimer, R. & Mariani, M.S. Integrating behavioural experimental findings into dynamical models to inform social change interventions.
# Nat Hum Behav 10, 873–883 (2026). https://doi.org/10.1038/s41562-026-02417-4
# we copied the package source into pkg/ to save your time (so that you do not need to fetch it from Zenodo)
if (!requireNamespace("conjointr", quietly = TRUE)) install.packages("pkg/conjointr_0.1.0.tar.gz", repos = NULL, type = "source")
