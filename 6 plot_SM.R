# Description: SM robustness-check plots comparing prompt variants within the same model (one set of plots per model).
# Input: prepared_sample .rda files per model (main + sm_* prompt variants), conjoint input attributes
# Output: PNG plots in plots/, combined PDFs in plots/

library(data.table)
library(dplyr)
library(igraph)
library(moments)
library(ggplot2)
library(diffuNet)
library(FinancialMath)
library(gridExtra)
library(grid)
library(tidyr)
library(patchwork)
library(cowplot)
library(ggridges)
library(doParallel)

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(file.path(project_root, "functions", "0_4 plot_function.R"))

# ============================================================================
# CONFIGURATION
# ============================================================================
exp_configs <- list("messaging_app", "energy_policy")
output_folder <- file.path(project_root,"plots");dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
base_size <- 45

model_configs <- list(
  gpt5     = list(color = "#336a5d", label = "GPT5"),
  claude   = list(color = "#d97757", label = "Claude")
)

MAIN_TEXT_LABEL <- "Baseline (main)"
prompt_levels <- c(MAIN_TEXT_LABEL, "MatchedSet", "ShuffledOrder", "Compressed", "NoMemory")

# extract a clean prompt-variant label
format_prompt_variant_name <- function(x) {
  sapply(x, function(name) {
    if (grepl("_prompt", name, ignore.case = TRUE)) {
      sub(".*_prompt", "", name)
    } else {
      MAIN_TEXT_LABEL
    }
  }, USE.NAMES = FALSE)
}

# fill color for one facet/panel per prompt variant
panel_colors_for <- function(prompt_variant_factor, model_color) {
  c("Human" = "#FFAD60",
    setNames(rep(model_color, nlevels(prompt_variant_factor) - 1),
             setdiff(levels(prompt_variant_factor), "Human")))
}

# ============================================================================
# make_sm_plots(): builds all three SM plots (importance, thresholds, partworth) 
# ============================================================================
make_sm_plots <- function(exp, model, model_color, model_label, output_folder){
  plot_store <- list()

  # ---- Shared inputs: this model's prepared_sample files, attributes, human reference ----
  data_folder <- file.path(project_root, "data", exp)
  input_folder <- file.path(project_root, "data", exp, "conjoint input")

  attributes <- fread(file.path(input_folder,"attributes_withNone.csv"))
  attributes[attribute=="adopters", attribute:="social_signal"]

  sample_name_list_all <- list.files(file.path(data_folder,"prepared_sample"))
  sample_name_list <- sample_name_list_all[grepl(paste0("_", model, "_temp"), sample_name_list_all, ignore.case = TRUE)]

  if (length(sample_name_list) == 0) {
    warning(paste0(exp, "/", model, ": no prepared_sample files found, skipping"))
    return(plot_store)
  }

  human_sample_file <- list.files(file.path(data_folder, "prepared_sample"), pattern = "_real\\.rda$")

  samples <- lapply(sample_name_list, function(sample_name) {
    load(file.path(data_folder, "prepared_sample", sample_name))
    sample
  })
  names(samples) <- sub("\\.rda$", "", sample_name_list)

  load(file.path(data_folder, "prepared_sample", human_sample_file))
  human_sample <- sample

  # ============================================================
  # BLOCK 1/3 -- VARIABLE IMPORTANCE (p_vi_sm)
  # ============================================================
  output <- foreach(raw_name = names(samples), .combine = "rbind") %do%{
    importance <- GetVariableImportance(fit.hb = samples[[raw_name]]$coefs, attributes = attributes)
    importance[, prompt_variant := format_prompt_variant_name(raw_name)]
    format_importance_variable_names(importance)
    importance
  }

  human_importance <- GetVariableImportance(fit.hb = human_sample$coefs, attributes = attributes)
  format_importance_variable_names(human_importance)
  human_importance[, prompt_variant := "Human"]

  output <- rbind(output, human_importance, use.names = TRUE)
  output$prompt_variant <- factor(output$prompt_variant,
                                   levels = c("Human", intersect(prompt_levels, unique(output$prompt_variant))))

  # order attributes by Human's own importance (social signal last)
  sorted_variable <- output[prompt_variant == "Human", .(mean), by = variable][order(mean), variable]
  output$variable <- factor(output$variable, levels = c(setdiff(sorted_variable,"Social signal"),"Social signal"))

  panel_colors <- panel_colors_for(output$prompt_variant, model_color)

  # Pearson correlation between each variant's and Baseline (main)'s importance values across attributes
  main_importance <- setNames(output[prompt_variant == "Baseline (main)"]$mean,
                               output[prompt_variant == "Baseline (main)"]$variable)
  corr_variants <- setdiff(levels(output$prompt_variant), c("Human", "Baseline (main)"))
  corr_labels <- rbindlist(lapply(corr_variants, function(v) {
    variant_importance <- setNames(output[prompt_variant == v]$mean, output[prompt_variant == v]$variable)
    r <- cor(main_importance[names(variant_importance)], variant_importance, method = "pearson")
    data.table(prompt_variant = v, x = nlevels(output$variable) +0.45, y = 0.7,
               label = sprintf("r = %.2f", r))
  }))
  corr_labels$prompt_variant <- factor(corr_labels$prompt_variant, levels = levels(output$prompt_variant))

  p_vi_sm <- ggplot(
    data = output,
    aes(x = variable, y = mean, fill = prompt_variant)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 0.4, linewidth = 1.5)+
    geom_text(data = corr_labels, aes(x = x, y = y, label = label), inherit.aes = FALSE,
              fontface = "bold", size = 12) +
    coord_flip() +
    labs(x=NULL,
         y=paste0("Importance score (", model_label, ", by prompt variant)"))+
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 0.9, 0.5)) +
    scale_fill_manual(values = panel_colors, breaks = levels(output$prompt_variant)) +
    facet_wrap(~prompt_variant, scales = "free_x", strip.position = "top", nrow = 1) +
    theme_paper(base_size = base_size) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 60),
      strip.text = element_text(size = 32)
    )

  if (exp == "energy_policy") {
    p_vi_sm <- p_vi_sm + theme(axis.title.x = element_text(colour = "transparent"))
  }

    plot_store[[paste0("p_vi_sm_", model, "_", exp)]] <- p_vi_sm
  plot_store[[paste0("p_vi_sm_colors_", model, "_", exp)]] <- panel_colors[levels(output$prompt_variant)]
  ggsave(file.path(output_folder,paste0(exp,"_SM_",model,"_variable importance.png")),
         plot = color_facet_strips(p_vi_sm, panel_colors[levels(output$prompt_variant)]),
         dpi = 100,  width = 22, height = 12)

  # ============================================================
  # BLOCK 2/3 -- ADOPTION THRESHOLDS (p_t_sm)
  # ============================================================
  output <- foreach(raw_name = names(samples), .combine = "rbind") %do%{
    data <- samples[[raw_name]]$thresholds
    data[, prompt_variant := format_prompt_variant_name(raw_name)]
    data
  }
  human_thresholds <- copy(human_sample$thresholds)
  human_thresholds[, prompt_variant := "Human"]
  output <- rbind(output, human_thresholds, use.names = TRUE)
  output$prompt_variant <- factor(output$prompt_variant,
                                   levels = c("Human", intersect(prompt_levels, unique(output$prompt_variant))))

  panel_colors_t <- panel_colors_for(output$prompt_variant, model_color)

  p_t_sm <- ggplot(output, aes(x = prompt_variant, y = threshold, fill = prompt_variant)) +
    stat_summary(fun = mean, geom = "bar", width = 0.7) +
    stat_summary(fun.data=mean_cl_normal, geom = "errorbar",width = 0.4, linewidth = 1.5,color = "black") +
    labs(x = NULL, y = "Threshold") +
    coord_cartesian(clip = "off")+
    scale_y_continuous(limits = c(0,1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
    scale_fill_manual(values = panel_colors_t) +
    theme_paper(base_size = base_size) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = base_size, angle = 30,hjust=1),
      plot.margin = margin(20, 50, 20, 50)
    )

  if (model == "claude") {
    p_t_sm <- p_t_sm + theme(axis.title.y = element_text(colour = "transparent"))
  }
  if (exp == "energy_policy") {
    p_t_sm <- p_t_sm + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  plot_store[[paste0("p_t_sm_", model, "_", exp)]] <- p_t_sm
  ggsave(file.path(output_folder,paste0(exp,"_SM_",model,"_thresholds_across prompt variants.png")), plot = p_t_sm, dpi = 100,  width = 15, height = 12)
  output$prompt_variant <- factor(output$prompt_variant, levels = rev(levels(output$prompt_variant)))

  mean_dt <- output[, .(mean_threshold = mean(threshold)), by = prompt_variant]
  mean_dt[, y_num := as.numeric(prompt_variant)]

  p_t_dist_sm <- ggplot(output, aes(x = threshold, y = prompt_variant, fill = prompt_variant)) +
    geom_density_ridges(alpha = 0.85, scale = 2, color = "black", linewidth = 0.4, from = 0, to = 1,
                         bandwidth = 0.02) +
    geom_segment(data = mean_dt, inherit.aes = FALSE,
                 aes(x = mean_threshold, xend = mean_threshold, y = y_num, yend = y_num + 1.8, color = prompt_variant),
                 linewidth = 1.3) +
    scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
    scale_fill_manual(values = panel_colors_t, breaks = levels(output$prompt_variant)) +
    scale_color_manual(values = panel_colors_t, breaks = levels(output$prompt_variant)) +
    labs(x = paste0("Threshold\n(", model_label, ", by prompt variant)"), y = NULL) +
    theme_paper(base_size = base_size) +
    theme(legend.position = "none")

  if (exp == "energy_policy") {
    p_t_dist_sm <- p_t_dist_sm + theme(axis.title.x = element_text(colour = "transparent"))
  }

  plot_store[[paste0("p_t_dist_sm_", model, "_", exp)]] <- p_t_dist_sm
  ggsave(file.path(output_folder,paste0(exp,"_SM_",model,"_thresholds distribution.png")), plot = p_t_dist_sm, dpi = 100, width = 8, height = 10)

  # ============================================================
  # BLOCK 3/3 -- PARTWORTH UTILITIES (p_partworth_sm)
  # ============================================================
  output <- foreach(raw_name = names(samples), .combine = "rbind") %do%{
    coefs <- samples[[raw_name]]$coefs
    data.fplot <- GetDataFPlot(coefs, predictors = colnames(coefs)[!colnames(coefs) %in% c("resp.id")])
    data.fplot[, prompt_variant := format_prompt_variant_name(raw_name)]
    data.fplot
  }
  human_partworth <- GetDataFPlot(human_sample$coefs, predictors = colnames(human_sample$coefs)[!colnames(human_sample$coefs) %in% c("resp.id")])
  human_partworth[, prompt_variant := "Human"]

  output <- rbind(output, human_partworth, use.names = TRUE)
  output$prompt_variant <- factor(output$prompt_variant,
                                   levels = c("Human", intersect(prompt_levels, unique(output$prompt_variant))))

  sorted_variable_pw <- output[prompt_variant == "Human"][order(mean), variable]
  output$variable <- factor(output$variable, levels = sorted_variable_pw)

  panel_colors_pw <- panel_colors_for(output$prompt_variant, model_color)

  p_partworth_sm <- ggplot(data = output,
               aes(x = variable, y = mean, ymin = lower, ymax = upper, color = prompt_variant)) +
    geom_pointrange(position = position_dodge(width = 0.2), size = 0.001) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, position = position_dodge(.9)) +
    coord_flip() +
    xlab(NULL) +
    ylab(paste0("Average partworth utility (", model_label, ", by prompt variant, 95% CI)")) +
    facet_wrap(~prompt_variant, strip.position = "top", nrow = 1) +
    scale_color_manual(values = panel_colors_pw, breaks = levels(output$prompt_variant)) +
    theme_paper(base_size = 35) +
    theme(legend.position = "none")
  if (exp == "energy_policy") {
    p_partworth_sm <- p_partworth_sm + theme(axis.title.x = element_text(colour = "transparent"))
  }
  plot_store[[paste0("p_partworth_sm_", model, "_", exp)]] <- p_partworth_sm
  plot_store[[paste0("p_partworth_sm_colors_", model, "_", exp)]] <- panel_colors_pw[levels(output$prompt_variant)]
  ggsave(file.path(output_folder,paste0(exp,"_SM_",model,"_partworth estimation.png")),
         plot = color_facet_strips(p_partworth_sm, panel_colors_pw[levels(output$prompt_variant)]),
         dpi = 100,  width = 22, height = 12)

  return(plot_store)
}


# BUILD PLOTS for every model x study combination
plot_store <- list()
for (model_name in names(model_configs)) {
  for (cfg in exp_configs) {
    plot_store <- c(plot_store, make_sm_plots(
      cfg,
      model = model_name,
      model_color = model_configs[[model_name]]$color,
      model_label = model_configs[[model_name]]$label,
      output_folder = output_folder
    ))
  }
}


# SHARED LEGEND (agent/model colors)
legend_labels_sm <- c("Human", sapply(model_configs, `[[`, "label"))
legend_df_sm <- data.table(
  model = factor(legend_labels_sm, levels = legend_labels_sm),
  x = 1, y = 1
)
legend_colors_sm <- setNames(c("#FFAD60", sapply(model_configs, `[[`, "color")), legend_labels_sm)

p_legend_sm <- ggplot(legend_df_sm, aes(x = x, y = y, fill = model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Agent type:", values = legend_colors_sm, breaks = names(legend_colors_sm)) +
  theme_paper(base_size = base_size, show_legend = TRUE) +
  theme_legend_bottom() +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

legend_grob_sm <- cowplot::get_legend(p_legend_sm)
legend_plot_sm <- cowplot::ggdraw(legend_grob_sm)

# -----------------------------------------------------------------
# SM (prompt variants): Thresholds (SM_combined_thresholds.pdf) 
# -----------------------------------------------------------------
p_gpt5_ps   <- plot_store[["p_t_sm_gpt5_energy_policy"]]
p_claude_ps <- plot_store[["p_t_sm_claude_energy_policy"]]
p_gpt5_aa   <- plot_store[["p_t_sm_gpt5_messaging_app"]]
p_claude_aa <- plot_store[["p_t_sm_claude_messaging_app"]]

tight_theme <- theme(plot.margin = margin(5, 5, 1, 5)) #margin(top, right, bottom, left)
p_gpt5_ps   <- p_gpt5_ps + tight_theme
p_claude_ps <- p_claude_ps + tight_theme
p_gpt5_aa   <- p_gpt5_aa + tight_theme
p_claude_aa <- p_claude_aa + tight_theme

grid <- plot_grid(
  p_gpt5_ps, p_claude_ps,
  p_gpt5_aa, p_claude_aa,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_x = -0.03, label_y = c(1.08, 1.08, 1.08, 1.08), label_size = 80,
  align = "hv", axis = "tblr"
)

grid <- plot_grid(NULL, grid, ncol = 2, rel_widths = c(0.06, 1))
grid <- plot_grid(NULL, grid, ncol = 1, rel_heights = c(0.03, 1))

grid <- ggdraw(grid) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.61, width = 0.042, height = 0.35) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.13, width = 0.042, height = 0.35) +
  draw_label("PS Experiment", x = 0.033, y = 0.78, angle = 90, fontface = "bold", size = 55) +
  draw_label("AA Experiment", x = 0.033, y = 0.3, angle = 90, fontface = "bold", size = 55)

grid <- plot_grid(grid, legend_plot_sm, ncol = 1, rel_heights = c(1, 0.06))

ggsave(
  file.path(output_folder, "SM_combined_thresholds.pdf"),
  grid, device = cairo_pdf, width = 30, height = 24 * 1.06, units = "in", limitsize = FALSE
)

# -----------------------------------------------------------------------------
# SM (prompt variants): Variable importance (SM_combined_variable_importance.pdf) 
# ------------------------------------------------------------------------------
p_gpt5_ps   <- plot_store[["p_vi_sm_gpt5_energy_policy"]]
p_claude_ps <- plot_store[["p_vi_sm_claude_energy_policy"]]
p_gpt5_aa   <- plot_store[["p_vi_sm_gpt5_messaging_app"]]
p_claude_aa <- plot_store[["p_vi_sm_claude_messaging_app"]]

p_gpt5_ps   <- p_gpt5_ps + tight_theme
p_claude_ps <- p_claude_ps + tight_theme
p_gpt5_aa   <- p_gpt5_aa + tight_theme
p_claude_aa <- p_claude_aa + tight_theme

p_gpt5_ps   <- color_facet_strips(p_gpt5_ps,   plot_store[["p_vi_sm_colors_gpt5_energy_policy"]])
p_claude_ps <- color_facet_strips(p_claude_ps, plot_store[["p_vi_sm_colors_claude_energy_policy"]])
p_gpt5_aa   <- color_facet_strips(p_gpt5_aa,   plot_store[["p_vi_sm_colors_gpt5_messaging_app"]])
p_claude_aa <- color_facet_strips(p_claude_aa, plot_store[["p_vi_sm_colors_claude_messaging_app"]])

grid <- plot_grid(
  p_gpt5_ps, p_claude_ps,
  p_gpt5_aa, p_claude_aa,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_x = 0.02, label_y = c(1.05, 1.05, 1.12, 1.12), label_size = 80,
  align = "hv", axis = "tblr"
)
grid$layers[[5]]$geom_params$ymax <- grid$layers[[5]]$geom_params$ymax + 0.0415
grid$layers[[7]]$geom_params$ymax <- grid$layers[[7]]$geom_params$ymax + 0.0415

grid <- plot_grid(NULL, grid, ncol = 2, rel_widths = c(0.06, 1))
grid <- plot_grid(NULL, grid, ncol = 1, rel_heights = c(0.03, 1))

grid <- ggdraw(grid) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.55, width = 0.04, height = 0.39) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.08, width = 0.04, height = 0.39) +
  draw_label("PS Experiment", x = 0.032, y = 0.74, angle = 90, fontface = "bold", size = 65) +
  draw_label("AA Experiment", x = 0.032, y = 0.27, angle = 90, fontface = "bold", size = 65)

grid <- plot_grid(grid, legend_plot_sm, ncol = 1, rel_heights = c(1, 0.06))

ggsave(
  file.path(output_folder, "SM_combined_variable_importance.pdf"),
  grid, device = cairo_pdf, width = 53, height = 24 * 1.06, units = "in", limitsize = FALSE
)

# ----------------------------------------------------------------------
# SM (prompt variants): Partworth (SM_combined_partworth.pdf) -------
# -----------------------------------------------------------------------
p_gpt5_ps   <- plot_store[["p_partworth_sm_gpt5_energy_policy"]]
p_claude_ps <- plot_store[["p_partworth_sm_claude_energy_policy"]]
p_gpt5_aa   <- plot_store[["p_partworth_sm_gpt5_messaging_app"]]
p_claude_aa <- plot_store[["p_partworth_sm_claude_messaging_app"]]

p_gpt5_ps   <- p_gpt5_ps + tight_theme
p_claude_ps <- p_claude_ps + tight_theme
p_gpt5_aa   <- p_gpt5_aa + tight_theme
p_claude_aa <- p_claude_aa + tight_theme

p_claude_ps <- p_claude_ps + scale_x_discrete(labels = NULL, breaks = NULL)
p_claude_aa <- p_claude_aa + scale_x_discrete(labels = NULL, breaks = NULL)

p_gpt5_ps   <- color_facet_strips(p_gpt5_ps,   plot_store[["p_partworth_sm_colors_gpt5_energy_policy"]])
p_claude_ps <- color_facet_strips(p_claude_ps, plot_store[["p_partworth_sm_colors_claude_energy_policy"]])
p_gpt5_aa   <- color_facet_strips(p_gpt5_aa,   plot_store[["p_partworth_sm_colors_gpt5_messaging_app"]])
p_claude_aa <- color_facet_strips(p_claude_aa, plot_store[["p_partworth_sm_colors_claude_messaging_app"]])

grid <- plot_grid(
  p_gpt5_ps, p_claude_ps,
  p_gpt5_aa, p_claude_aa,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_x = 0.02, label_y = c(1.05, 1.05, 1.05, 1.05), label_size = 80,
  align = "hv", axis = "tb"
)

grid <- plot_grid(NULL, grid, ncol = 2, rel_widths = c(0.06, 1))
grid <- plot_grid(NULL, grid, ncol = 1, rel_heights = c(0.03, 1))

grid <- ggdraw(grid) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.53, width = 0.045, height = 0.42) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.06, width = 0.045, height = 0.42) +
  draw_label("PS Experiment", x = 0.035, y = 0.74, angle = 90, fontface = "bold", size = 55) +
  draw_label("AA Experiment", x = 0.035, y = 0.27, angle = 90, fontface = "bold", size = 55)

grid <- plot_grid(grid, legend_plot_sm, ncol = 1, rel_heights = c(1, 0.06))

ggsave(
  file.path(output_folder, "SM_combined_partworth.pdf"),
  grid, device = cairo_pdf, width = 53, height = 24 * 1.06, units = "in", limitsize = FALSE
)

#-----------------------------------------------------------------------------------
#SM (prompt variants): Threshold distribution (SM_combined_threshold_distribution.pdf)
#-----------------------------------------------------------------------------------
p_gpt5_ps   <- plot_store[["p_t_dist_sm_gpt5_energy_policy"]]
p_claude_ps <- plot_store[["p_t_dist_sm_claude_energy_policy"]]
p_gpt5_aa   <- plot_store[["p_t_dist_sm_gpt5_messaging_app"]]
p_claude_aa <- plot_store[["p_t_dist_sm_claude_messaging_app"]]

p_gpt5_ps   <- p_gpt5_ps + tight_theme
p_claude_ps <- p_claude_ps + tight_theme
p_gpt5_aa   <- p_gpt5_aa + tight_theme
p_claude_aa <- p_claude_aa + tight_theme

grid <- plot_grid(
  p_gpt5_ps, p_claude_ps,
  p_gpt5_aa, p_claude_aa,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_x = -0.03, label_y = c(1.08, 1.08, 1.08, 1.08), label_size = 80,
  align = "hv", axis = "tblr"
)

grid <- plot_grid(NULL, grid, ncol = 2, rel_widths = c(0.06, 1))
grid <- plot_grid(NULL, grid, ncol = 1, rel_heights = c(0.03, 1))

grid <- ggdraw(grid) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.61, width = 0.042, height = 0.35) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.13, width = 0.042, height = 0.35) +
  draw_label("PS Experiment", x = 0.033, y = 0.78, angle = 90, fontface = "bold", size = 55) +
  draw_label("AA Experiment", x = 0.033, y = 0.3, angle = 90, fontface = "bold", size = 55)

grid <- plot_grid(grid, legend_plot_sm, ncol = 1, rel_heights = c(1, 0.06))

ggsave(
  file.path(output_folder, "SM_combined_threshold_distribution.pdf"),
  grid, device = cairo_pdf, width = 30, height = 24 * 1.06, units = "in", limitsize = FALSE
)

