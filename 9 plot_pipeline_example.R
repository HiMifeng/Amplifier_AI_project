# Description: create plot pipeline
# Step 1: simulate network diffusion (mixed real/AI respondents, one seeding strategy) over
#   a single AddHealth network, for one AI model, one product profile, and 6 AI-proportion values.
# Step 2: draw threshold distribution, network, diffusion used in this example

# Input: prepared_sample .rda files, data/network_addhealth/ network CSVs + sampled_nets.rda
# Output: data/diffusion_example.csv, plus three illustrative figures in plots/:
#   pipeline_example_threshold (Human vs LLM threshold distribution, for the
#   profile used below), pipeline_example_network_q00/q06 (network panels at
#   q=0/q=0.6), and pipeline_example_diffusion (adoption-rate curves for those
#   two q values)

library(igraph)
library(data.table)
library(dplyr)
library(moments)
library(diffuNet)
library(FinancialMath)
library(ids)
library(ggplot2)
set.seed(123)

# setting
# ================================================================================
exp="messaging_app"

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)

source(file.path(project_root,"functions",'0_2 seeding_function.R'))
source(file.path(project_root,"functions",'0_3 diffusion_function.R'))

data_folder <- file.path(project_root,"data",exp)

output_folder <- file.path(project_root,"data")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
fig_folder <- file.path(project_root, "plots")
dir.create(fig_folder, recursive = TRUE, showWarnings = FALSE)

# get prepared sample files
sample_data_folder <-  paste0(file.path(data_folder, "prepared_sample"))
sample_files <-list.files(sample_data_folder)
real_sample_file <- grep("real",sample_files,value=TRUE)
ai_sample_file <- "app_gpt3.5_temp1.rda"

# load net file -- smallest of the 18 sampled networks (25 nodes), only network used here
net_folder <- file.path(project_root,"data","network_addhealth","AddHealth_Networks_Largest_Components")
net_file <- "addhealth_net_75.csv"


# ===============================================================
# -------------------run diffusion example---------------------
# ===============================================================
r_seeds = 0.01
mapping_time = 1
ai_percents =c(0,0.2,0.4,0.6,0.8,1)

# load empirical net
# ------------------------------------------------------------
gmat <- fread(file.path(net_folder,net_file)) %>% as.matrix()
rownames(gmat)<-colnames(gmat)
g<-graph_from_adjacency_matrix(gmat,mode="undirected")
n <- nrow(gmat)
n_seeds <- ceiling(n*r_seeds)
ids <- colnames(gmat)

# data loading and preprocessing
# ------------------------------------------------------------
# load sample
load(file = file.path(sample_data_folder, real_sample_file))
real_sample <- copy(sample)
load(file = file.path(sample_data_folder, ai_sample_file))
ai_sample <- copy(sample)

# sampling from real participants and ai
for (ai_percent in ai_percents){

  mixed_sample <- mix_sample(n, real_sample, ai_sample, ai_percent = ai_percent)
  mixed_ids <- mixed_sample$coefs$mix_id

  # record net_sample_id
  net_sample_id <- ids::uuid(1,drop_hyphens = TRUE,use_time = TRUE)

  # ----------------mapping---------------------------------
  mapping <- sample(1:n, n)

  # record mapping dictionary
  mapping_dic <- data.table(node_id = ids, mix_id = mixed_ids[mapping])
  # get mapped_coefs
  coefs <- merge(mapping_dic, mixed_sample$coefs, by = 'mix_id', all.x = TRUE, allow.cartesian = TRUE)# key_x might duplicate, key_y is unique
  coefs$mix_id <- NULL; coefs$resp.id <- NULL;
  coefs <- coefs %>% setnames(old='node_id', new='resp.id') %>% .sort_ids(sort_on = ids, get="datatable")
  # get profiles_matrix
  profiles_matrix <- mixed_sample$profiles_matrix
  # single product profile for this example
  profile_name <- "web|simple|low|multi_person"

  product_profile <- t(profiles_matrix[profile_name,]) %>% as.data.table()
  product_profile$profile <- profile_name
  setcolorder(product_profile, c("profile", names(product_profile)[!names(product_profile) %in% "profile"]))
  product_profile$profile <- 1

  # find seeds for the seeding strategy
  #--------------select top nodes------------------
  print("start seed set selection")
  simple_centralities_df <- get_simple_centralities(g)

  # one entry per seeding strategy -- add/remove a strategy by editing only this list
  top_ids_list <- list(
    degree      = ids[find_seeds_index(n_seeds, simple_centralities_df[['degree']])]
  )
  seed_sets <- lapply(top_ids_list, seedIds2dt, ids = ids, profile_name = profile_name)

  #--------------run diffusion------------------
  print("running diffusions")
  results <- lapply(seed_sets, function(seeds) {
    diffuNet::run_diffusion(g = gmat, n_product = 1, seeds = seeds, decisionOpportunity = TRUE,
                             exposureCon = TRUE, deviation = FALSE, t_max = NULL,
                             stop_on_stable_state = 2, social_value = "ratio",
                             decision_function = utility_based_decision,
                             product_profile = product_profile, coefs)
  })

  new_adopters_flow_list <- lapply(results, function(r) {
    flow <- c(sum(r$adoption_matrix[, 1]), r$new_adopters_flow[1, ])
    paste(flow, collapse = ",")
  })
  accumulated_adopters_flow_list <- lapply(results, function(r) {
    paste(colSums(r$adoption_matrix), collapse = ",")
  })

  # flatten each named list 
  strategy_cols <- c(
    suffix_named(new_adopters_flow_list, "new_adopters_flow"),
    suffix_named(accumulated_adopters_flow_list, "accumulated_adopters_flow")
  )

  rs <- data.table(net_file=net_file,n=n,
                   real_sample_file=real_sample_file,
                   ai_sample_file=ai_sample_file,
                   net_sample_id=net_sample_id,
                   ai_percent=ai_percent,
                   mapping_times =mapping_time
  )
  rs <- cbind(rs, as.data.table(strategy_cols))

  append2csv(output_folder, filename='diffusion_example.csv', rs)
}

# =========================================================================
# plot: threshold distribution -- Human vs LLM
# data: for the profile used to run the diffusion above 
# (profile_name, real_sample/ai_sample are the same objects  from in the loop above)
# =========================================================================

col_human <- "#FFAD60"; col_llm <- "gray"    # same palette as the other figures

th_human <- real_sample$thresholds[real_sample$thresholds$profile == profile_name, ]$threshold
th_llm   <- ai_sample$thresholds[ai_sample$thresholds$profile == profile_name, ]$threshold

draw_threshold_panel <- function(x, col, title, show_xlab = TRUE) {
  breaks <- seq(0, 1, 0.1)
  share <- hist(x, breaks = breaks, plot = FALSE)$counts / length(x)
  
  par(mar = c(4, 4.5, 2.2, 1), las = 1, cex.lab = 1.5, cex.main = 1.6, font.main = 2)
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       xlab = if (show_xlab) "Threshold" else "", ylab = "Share of respondents",
       main = title, axes = FALSE)
  rect(breaks[-length(breaks)], 0, breaks[-1], share, col = col, border = NA)
  axis(1, at = seq(0, 1, 0.2), cex.axis = 1.6)
  axis(2, at = seq(0, 1, 0.2), cex.axis = 1.6)
  box()
}

draw_threshold_fig <- function() {
  par(mfrow = c(2, 1))
  draw_threshold_panel(th_human, col_human, "Human", show_xlab = FALSE)
  draw_threshold_panel(th_llm, col_llm, "LLM")
}

threshold_outfile <- file.path(fig_folder, "pipeline_example_threshold")

pdf(paste0(threshold_outfile, ".pdf"), width = 5, height = 6.5, useDingbats = FALSE)
draw_threshold_fig()
dev.off()

png(paste0(threshold_outfile, ".png"), width = 5, height = 6.5, units = "in", res = 600)
draw_threshold_fig()
dev.off()

# =========================================================================
# ---------- plot network panels: addhealth_net_75.csv--------------------
# =========================================================================
# network setting
# ------------------------------------------
q_values <- c(0, 0.6)    # q: ai percentage
# col_human/col_llm defined once above, reused here
show_title  <- TRUE
show_legend <- TRUE 
A <- as.matrix(read.csv(file.path(net_folder, net_file), check.names = FALSE))
g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
n <- vcount(g)

# same layout as the plain network figure
set.seed(9)
coords <- layout_with_gem(g)

# draw 
# -------------------------------------------------------
draw_network_fig <- function(q) {
  k <- round(q * n)      # number of artificial agents
  V(g)$shape <- rep("circle", n)
  V(g)$color <- col_human
  if (k > 0) {         # 1:k would misbehave when k is 0
    V(g)$shape[1:k] <- "square"
    V(g)$color[1:k] <- col_llm
  }

  par(mar = c(if (show_legend) 3.4 else 0.4, 0.4,
              if (show_title)  2.6 else 0.4, 0.4),
      cex.main = 1.6, font.main = 2)
  plot(g, layout = coords,
       vertex.size  = 15,
       vertex.label = NA,
       vertex.color = V(g)$color,
       vertex.shape = V(g)$shape,
       vertex.frame.color = "grey20",
       edge.width   = 0.7,
       edge.color   = adjustcolor("black", alpha.f = 0.6),
       main = if (show_title) bquote(italic(q) == .(q)) else NA)

  if (show_legend) {
    keep <- c(k < n, k > 0)             # human entry, LLM entry
    # placed in user coordinates below the layout, which spans roughly -1..1,
    # so it clears the lowest node instead of sitting on top of it
    par(xpd = NA)
    legend(x = 0, y = -1.20, xjust = 0.5,
           legend = c("Human", "LLM")[keep],
           pch    = c(21, 22)[keep],
           pt.bg  = c(col_human, col_llm)[keep],
           col = "grey20", pt.cex = 1.9, cex = 1.3, bty = "n", horiz = TRUE)
    par(xpd = FALSE)
  }
}

for (q in q_values) {
  net_outfile <- file.path(fig_folder, sprintf("pipeline_example_network_q%02d", round(q * 10)))

  pdf(paste0(net_outfile, ".pdf"), width = 5, height = 5, useDingbats = FALSE)
  draw_network_fig(q)
  dev.off()

  png(paste0(net_outfile, ".png"), width = 5, height = 5, units = "in", res = 600)
  draw_network_fig(q)
  dev.off()
}

# =========================================================================
# --------plot: adoption on the AddHealth network (n = 25)--------------
# =========================================================================

# Data: diffusion_example.csv (written by the loop above)
# Two single trials shown: q = 0 (all human) vs q = 0.6 (LLM-human mix)
# col_human/col_llm defined once above, reused here (col_llm = the q=0.6 mixed line)
diffusion_outfile <- file.path(fig_folder, "pipeline_example_diffusion")

# data
# -----------------------------------------------------------------
# diffusion_example.csv has exactly one row per ai_percent (one run of the loop above)
d_curve <- read.csv(file.path(output_folder, "diffusion_example.csv"))
n_curve <- d_curve$n[1]

pick_run <- function(pct) {
  row <- d_curve[d_curve$ai_percent == pct, ]
  as.numeric(strsplit(row$accumulated_adopters_flow_degree[1], ",")[[1]])
}

v_human <- pick_run(0)
v_mixed <- pick_run(0.6)

# A cascade that stops has no further adopters
Tmax <- max(length(v_human), length(v_mixed))
pad  <- function(v) c(v, rep(v[length(v)], Tmax - length(v))) / n_curve

tt      <- seq_len(Tmax)
y_human <- pad(v_human)
y_mixed <- pad(v_mixed)

# draw 
# -----------------------------------------------------------------
# monotone spline: smooths the curve
smooth_line <- function(y, col) {
  xs <- seq(1, Tmax, length.out = 400)
  lines(xs, splinefun(tt, y, method = "hyman")(xs), col = col, lwd = 2.4)
}

draw_diffusion_fig <- function() {
  par(mar = c(4.2, 4.4, 1.2, 1.2), mgp = c(2.6, 0.7, 0), las = 1, cex.lab = 1.3)
  plot(NA,
       xlim = c(1, Tmax), ylim = c(0, 1),
       xaxs = "i", yaxs = "i",
       xlab = "Time", ylab = "Adoption rate",
       axes = FALSE)

  smooth_line(y_human, col_human)
  smooth_line(y_mixed, col_llm)

  axis(1, at = tt, tcl = -0.3, cex.axis = 1.2)
  axis(2, at = seq(0, 1, 0.2), tcl = -0.3, cex.axis = 1.2)
  box()                                    # closed frame on all four sides

  legend("topleft",
         legend = c(expression(italic(q) == 0), expression(italic(q) == 0.6)),
         col    = c(col_human, col_llm),
         lwd    = 2.4, bty = "n", inset = c(0.02, 0.03), seg.len = 1.8, cex = 1.3)
}

pdf(paste0(diffusion_outfile, ".pdf"), width = 5, height = 4, useDingbats = FALSE)
draw_diffusion_fig()
dev.off()

png(paste0(diffusion_outfile, ".png"), width = 5, height = 4, units = "in", res = 600)
draw_diffusion_fig()
dev.off()

