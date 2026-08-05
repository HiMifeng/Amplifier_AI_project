# Description: Two LaTeX summary tables: (1) descriptive statistics for the 18 real AddHealth
#   networks, and (2) demographic characteristics of real respondents
# Input: data/network_addhealth/sampled_nets.rda,
#   data/network_addhealth/AddHealth_Networks_Largest_Components/,
#   data/{messaging_app,energy_policy}/demographics/*_real.csv
# Output: tables/network_summary.tex, tables/demographics_summary.tex

library(igraph)
library(qgraph)
library(latex2exp)
library(data.table)
library(dplyr)

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)

##############################################################################
################### 18-network statistics + LaTeX table ####################
##############################################################################
suppressPackageStartupMessages(library(igraph))

# --- paths -------------------------------------------------------------------
data_dir   <- file.path(project_root, "data", "network_addhealth")
net_folder <- file.path(data_dir, "AddHealth_Networks_Largest_Components")
tables_folder <- file.path(project_root, "tables")
dir.create(tables_folder, recursive = TRUE, showWarnings = FALSE)
out_file   <- file.path(tables_folder, "network_summary.tex")

set.seed(20260722)  # reproducible Louvain community detection

# --- load the 18-network sample ---------------------------------------------
# sampled_nets: named list of 9 size x clustering categories.
load(file.path(data_dir, "sampled_nets.rda"))
net_files <- unlist(sampled_nets, use.names = FALSE)

# --- per-network statistics --------------------------------------------------
read_net <- function(file) {
  m <- as.matrix(read.csv(file.path(net_folder, file), check.names = FALSE))
  dimnames(m) <- NULL
  graph_from_adjacency_matrix(m, mode = "undirected", diag = FALSE)
}

analyze_network <- function(g) {
  data.frame(
    N             = vcount(g),                                    # number of nodes
    E             = ecount(g),                                    # number of edges
    mean_deg      = 2 * ecount(g) / vcount(g),                    # average degree
    density       = edge_density(g),                             # fraction of realised ties
    avg_clust     = transitivity(g, type = "average"),           # mean local clustering
    mean_path     = mean_distance(g),                            # average shortest-path length
    diameter      = diameter(g),                                 # longest shortest path
    modularity    = modularity(cluster_louvain(g)),              # Louvain modularity Q
    assortativity = assortativity_degree(g, directed = FALSE)    # degree assortativity r
  )
}

stats <- do.call(rbind, lapply(net_files, function(f) analyze_network(read_net(f))))

# --- aggregate across the 18 networks ---------------------------------------
agg <- data.frame(
  mean = sapply(stats, mean),
  sd   = sapply(stats, sd),
  min  = sapply(stats, min),
  max  = sapply(stats, max)
)

# --- build the LaTeX summary table ------------------------------------------
fmt <- function(x, d) formatC(x, format = "f", digits = d)

# row labels, and decimals for the mean (SD) cell and for the min--max range
row_lab <- c(N = "Number of nodes",     E = "Number of edges",
             mean_deg = "Average degree", density = "Density",
             avg_clust = "Average clustering", mean_path = "Average path length",
             diameter = "Diameter", modularity = "Modularity",
             assortativity = "Degree assortativity")
dec_ms <- c(N = 0, E = 0, mean_deg = 1, density = 3, avg_clust = 2,
            mean_path = 2, diameter = 1, modularity = 2, assortativity = 2)
dec_rg <- c(N = 0, E = 0, mean_deg = 1, density = 3, avg_clust = 2,
            mean_path = 2, diameter = 0, modularity = 2, assortativity = 2)

srows <- vapply(names(row_lab), function(s) {
  sprintf("        %-22s & %s (%s) & %s--%s \\\\",
          row_lab[s],
          fmt(agg[s, "mean"], dec_ms[s]), fmt(agg[s, "sd"], dec_ms[s]),
          fmt(agg[s, "min"],  dec_rg[s]), fmt(agg[s, "max"], dec_rg[s]))
}, character(1))

table_tex <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
  "    \\caption{Summary of the structural characteristics of the 18 empirical Add Health networks. Each cell reports the mean across the 18 networks with the standard deviation in parentheses; the last column gives the range (minimum--maximum).}",
  "    \\label{stab:network-summary}",
  "    \\begin{tabular*}{0.5\\textwidth}{@{\\extracolsep{\\fill}} l c c}",
  "        \\hline\\hline",
  "        Statistic & Mean (SD) & Range \\\\",
  "        \\hline",
  srows,
  "        \\hline\\hline",
  "    \\end{tabular*}",
  "\\end{table}"
)

writeLines(table_tex, out_file)
cat(table_tex, sep = "\n")
cat("\n")

##############################################################################
############### Real-respondent demographics summary table #################
##############################################################################

demo_out_file <- file.path(tables_folder, "demographics_summary.tex")

demo <- list(
  messaging_app = fread(file.path(project_root, "data", "messaging_app", "demographics", "app_real.csv")),
  energy_policy = fread(file.path(project_root, "data", "energy_policy", "demographics", "energypolicy_real.csv"))
)

age_labels <- c(`1` = "18-24", `2` = "25-34", `3` = "35-44", `4` = "45-54", `5` = "55-64",
                 `6` = "65-74", `7` = "75 or above")
gender_labels <- c(`1` = "Male", `2` = "Female", `3` = "Other")
education_labels <- c(`1` = "Less than high school degree",
                       `2` = "High school graduate",
                       `3` = "Some college but no degree",
                       `4` = "Associate degree in college",
                       `5` = "Bachelor's degree in college",
                       `6` = "Master's degree",
                       `7` = "Doctoral degree",
                       `8` = "Professional degree")
subj_labels <- c(`1` = "Economics",
                  `2` = "Humanities",
                  `3` = "Science",
                  `4` = "None of the above")
income_labels <- c(`1` = "Under \\$25,000", `2` = "\\$25,001 - \\$49,999", `3` = "\\$50,000 - \\$74,999",
                    `4` = "\\$75,000 - \\$99,999", `5` = "\\$100,000 - \\$149,999",
                    `6` = "\\$150,000 - \\$249,999", `7` = "\\$250,000 and over")
politics_labels <- c(`1` = "Conservative and nationalist",
                      `2` = "Liberal and anti-traditional",
                      `3` = "None of above")

# --- N (%) helper: count of resp with a given code, out of that experiment's sample ----------
n_pct <- function(col, code) {
  if (is.null(col)) return(NA_character_)
  n <- sum(col == code, na.rm = TRUE)
  pct <- 100 * n / length(col)
  sprintf("%d (%s\\%%)", n, formatC(pct, format = "f", digits = 1))
}

# --- build the category rows for one demographic variable ------------------------------------
demo_block <- function(colname, labels, header) {
  aa_col <- demo$messaging_app[[colname]]
  ps_col <- demo$energy_policy[[colname]]
  rows <- sprintf("        \\multicolumn{3}{l}{\\textit{%s}} \\\\", header)
  for (code in names(labels)) {
    aa <- n_pct(aa_col, as.integer(code))
    ps <- n_pct(ps_col, as.integer(code))
    if (identical(aa, "0 (0.0\\%)") && identical(ps, "0 (0.0\\%)")) next  # unused level in both samples
    rows <- c(rows, sprintf("        \\quad %-30s & %-14s & %-14s \\\\",
                             labels[[code]], ifelse(is.na(aa), "--", aa), ifelse(is.na(ps), "--", ps)))
  }
  rows
}

demo_rows <- c(
  demo_block("Age", age_labels, "Age"),
  demo_block("Gender", gender_labels, "Gender"),
  demo_block("Education", education_labels, "Education"),
  demo_block("subj", subj_labels, "Field of study"),
  demo_block("Income", income_labels, "Household income"),
  demo_block("politcs", politics_labels, "Political orientation (energy\\_policy only)")
)

demo_table_tex <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
  "    \\caption{Demographic characteristics of the real (human) respondents in each experiment. Each cell reports the number of respondents in that category, with the percentage of that experiment's sample in parentheses. Political orientation was collected for the energy\\_policy experiment only.}",
  "    \\label{stab:demographics-summary}",
  sprintf("    \\begin{tabular*}{0.75\\textwidth}{@{\\extracolsep{\\fill}} l c c}"),
  "        \\hline\\hline",
  sprintf("        & AA Experiment (N=%d) & PS Experiment (N=%d) \\\\",
          nrow(demo$messaging_app), nrow(demo$energy_policy)),
  "        \\hline",
  demo_rows,
  "        \\hline\\hline",
  "    \\end{tabular*}",
  "\\end{table}"
)

writeLines(demo_table_tex, demo_out_file)
cat(demo_table_tex, sep = "\n")
cat("\n")

