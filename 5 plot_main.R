# Description: Generate final plots (variable importance, thresholds, adoption rate, partworth) for both studies.
# This script only cover the data generated with "basedline prompt"
# Input: prepared_sample .rda + diffusion result CSVs, conjoint input attributes
# Output: PNG/PDF plots in plots/

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


# configs 
# ====================================
exp_configs <- list("messaging_app", "energy_policy")
output_folder <- file.path(project_root,"plots");dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
make_plots <- function(exp, output_folder){
  plot_store<-list()
  # --------------------load the data ----------------------------

  exp_label = ifelse(exp=="messaging_app","AA experiment","PS experiment")

  data_folder =file.path(project_root, "data", exp)
  input_folder = file.path(project_root, "data", exp, "conjoint input")

  attributes<- fread(file.path(input_folder,"attributes_withNone.csv"))
  attributes[attribute=="adopters", attribute:="social_signal"]
  diffusion_results = list.files(file.path(data_folder,"diffusion" ) )
  
  output_collective <- fread(file = file.path(data_folder,"diffusion",paste0(exp,"_diffusion_result.csv")))
  sample_name_list_all <- list.files(file.path(data_folder,"prepared_sample"))
  # main-text plots only: exclude the "sm_" (SM robustness-check prompt variants) and "test_"
  # (old-vs-new prompt wording test files) prepared_sample files, keeping only the main
  # ChoiceRandomized files (and the real human data).
  sample_name_list <- sample_name_list_all[!grepl("^sm_|^test_", sample_name_list_all)]

  #---------------------------- plot setting----------------------------

  # create color maps
  formatted_sample_names<-format_sample_name(sample_name_list)
  sample_colors <- c(
    setNames(rep("#FFAD60", length(grep("Human", formatted_sample_names, value = TRUE))),
             grep("Human", formatted_sample_names, value = TRUE)),
    setNames(rep("#336a5d", length(grep("GPT5", formatted_sample_names, value = TRUE))),
             grep("GPT5", formatted_sample_names, value = TRUE)), ##225649
    setNames(rep("#75ac9e", length(grep("GPT3.5", formatted_sample_names, value = TRUE))),
             grep("GPT3.5", formatted_sample_names, value = TRUE)),
    setNames(rep("#4e6bfd", length(grep("Deepseek", formatted_sample_names, value = TRUE))),
             grep("Deepseek", formatted_sample_names, value = TRUE)),
    setNames(rep("#d97757", length(grep("Claude", formatted_sample_names, value = TRUE))),
             grep("Claude", formatted_sample_names, value = TRUE)),
    setNames(rep("#a273ba", length(grep("Gemini", formatted_sample_names, value = TRUE))),
             grep("Gemini", formatted_sample_names, value = TRUE))
  )
  
  AI_colors <- sample_colors[!grepl("Human", names(sample_colors))]
  # text size
  base_size= 45

  #---------------plot: factor importance------------------------
  output <- foreach(sample_name = sample_name_list,.combine = "rbind") %do%{
    load(file.path(data_folder,"prepared_sample",sample_name) )
    data= sample$coefs 
    importance <- GetVariableImportance(fit.hb = data,attributes = attributes)
    importance$sample_name <-  gsub(".rda","",sample_name) 
    # format sample name
    raw_name <- sub("\\.rda$", "", sample_name)
    importance[, sample_name := format_sample_name(raw_name)] 
    
    # format variable
    format_importance_variable_names(importance)
    importance
  }

  output$sample_name <- factor(output$sample_name, levels = sample_name_levels(output$sample_name))

  # sort variable: put social signal at the begining and the other variables in a descending order of importance
  sorted_variable <-output[grepl("Human",sample_name), .(mean), by = variable][order(mean), variable]
  print(sorted_variable)
  output$variable <- factor(output$variable, levels = c(setdiff(sorted_variable,"Social signal"),"Social signal"))
  
  p_vi <- ggplot(
    data = output,
    aes(x = variable, y = mean, fill = sample_name)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper),width = 0.4, linewidth = 1.5)+
    coord_flip() +
    labs(x=NULL,
         y="Importance score by agent")+
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 0.9, 0.5)) +
    scale_fill_manual(values = sample_colors, breaks = levels(output$sample_name)) +
    facet_wrap(~sample_name, scales = "free_x", strip.position = "top", nrow = 1) +
    theme_paper()+
    theme(
      strip.text = element_blank(),
      strip.background = element_blank()
    )
  
  if (exp == "energy_policy") {
    p_vi =p_vi + theme(axis.title.x = element_text(colour = "transparent"))
  } 
  
  p_vi
  plot_store[[paste0("p_vi_", exp)]] <- p_vi
  ggsave(file.path(output_folder,paste0(exp,"_variable importance.png")), plot = p_vi, dpi = 100,  width = 22, height = 12)
  
  
  #----------------plot individual level: plot_thresholds-------------------
  output <- foreach(sample_name = sample_name_list,.combine = "rbind") %do%{
    load(file.path(data_folder,"prepared_sample",sample_name) )
    data= sample$thresholds
    data$sample_name= gsub(".rda","",sample_name)
    # format sample name
    raw_name <- sub("\\.rda$", "", sample_name)
    data[, sample_name := format_sample_name(raw_name)] 
    data
  }
  output$sample_name <- factor(output$sample_name, levels = sample_name_levels(output$sample_name))

  #output[,mean_threshold_by_profile_expname:=mean(threshold), by=.(profile,sample_name)]
  # 
  p_t<- ggplot(output, aes(x = sample_name, y = threshold, fill = sample_name)) +
    stat_summary(fun = mean, geom = "bar", width = 0.7) +
    stat_summary(fun.data=mean_cl_normal, geom = "errorbar",width = 0.4, linewidth = 1.5,color = "black") +
    labs(#title = paste0("Average threshold across different agents in the ", exp_label),
      x =NULL,#"Agent type",#  #
      y = "Threshold"
      #fill="Agent type"
    ) +
    coord_cartesian(clip = "off")+
    scale_y_continuous(limits = c(0,1), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
    scale_fill_manual( name = "Agent type:", values = sample_colors) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE )  )+
    theme_paper()+
    theme(
      axis.text.x = element_text(size = base_size, angle = 30,hjust=1),
      plot.margin = margin(20, 50, 20, 50)
    )
  
  p_t_legend <- p_t +
    theme_legend_bottom() +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE) ,
           color = "none", linetype = "none",
           label.theme = element_text(margin = margin(l = 2, r = 8))
    )

  
  if (exp == "energy_policy") {
    p_t <- p_t + theme(axis.text.x = element_text(colour = "transparent"))
    legend_grob <- cowplot::get_legend(p_t_legend)
    legend_plot <- cowplot::ggdraw(legend_grob)
    plot_store[[paste0("legend_plot_", exp)]] <- legend_plot
  }
  
  plot_store[[paste0("p_t_", exp)]] <- p_t
  ggsave(file.path(output_folder,paste0(exp,"_thresholds_across agent types.png")), plot = p_t, dpi = 100,  width = 15, height = 12)


  #------------------plot individual level: threshold distribution-----------------
  output$sample_name <- factor(output$sample_name, levels = rev(levels(output$sample_name)))
  mean_dt <- output[, .(mean_threshold = mean(threshold)), by = sample_name]
  mean_dt[, y_num := as.numeric(sample_name)]

  p_t_dist <- ggplot(output, aes(x = threshold, y = sample_name, fill = sample_name)) +
    geom_density_ridges(alpha = 0.85, scale = 2, color = "black", linewidth = 0.4, from = 0, to = 1,
                         bandwidth = 0.02) +
    geom_segment(data = mean_dt, inherit.aes = FALSE,
                 aes(x = mean_threshold, xend = mean_threshold, y = y_num, yend = y_num + 1.8, color = sample_name),
                 linewidth = 1.3) +
    scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0), breaks = seq(0, 1, 0.2)) +
    scale_fill_manual(values = sample_colors, breaks = levels(output$sample_name)) +
    scale_color_manual(values = sample_colors, breaks = levels(output$sample_name)) +
    labs(x = "Threshold", y = NULL) +
    theme_paper() +
    theme(legend.position = "none")

  if (exp == "energy_policy") {
    p_t_dist <- p_t_dist + theme(axis.title.x = element_text(colour = "transparent"))
  }

  plot_store[[paste0("p_t_dist_", exp)]] <- p_t_dist
  ggsave(file.path(output_folder,paste0(exp,"_thresholds distribution.png")), plot = p_t_dist, dpi = 100, width = 6, height = 10)


  #---------------------- collective level: adoption rate---------------------
  output<-output_collective
  output[,diffusion_id :=1:nrow(output)]
  output[,`AI type`:= format_sample_name(ai_sample_file)]
  
  # dataset format from wide to long
  long_output <- output %>%
    pivot_longer(cols = starts_with("profit"),
                 names_to = "seeding_type",
                 values_to = "profit") %>% as.data.table()
  long_output$seeding_type <- gsub("profit_", "", long_output$seeding_type)
  long_output <- long_output[, .(net_file, n, real_sample_file, `AI type`, ai_percent, profile_name, seeding_type, profit, thresholds_p_value0, thresholds_p_value1, thresholds_p_mean, diffusion_id)]
  
  # compute the usable value
  # threshold; group by
  long_output[,mean_threshold_p_by_ai_percent:=mean(thresholds_p_mean), by=.(`AI type`,ai_percent)] 
  long_output[, mean_thresholds_p_value0_by_ai_percent:=mean(thresholds_p_value0),by=.(`AI type`,ai_percent)] 
  long_output[, mean_thresholds_p_value1_by_ai_percent:=mean(thresholds_p_value1),by=.(`AI type`,ai_percent)] 
  
  # profit
  long_output[,profit:=profit/n]
  
  # profit: groupby
  long_output[, mean_profit_by_ai_percent := mean(profit), by = .(`AI type`,ai_percent,seeding_type)]
  long_output[, sd_profit_by_ai_percent:= sd(profit), by = .(`AI type`,ai_percent,seeding_type)]
  long_output[, count_by_ai_percent := .N, by = .(`AI type`,ai_percent, seeding_type)]
  
  # structure
  long_output<-long_output[ai_percent%in% seq(from=0,to=1, by=0.1),]
  
  # plot: adoption rate
  p_adoptionrate<-ggplot(long_output[seeding_type%in% c("rand","degree"),],
                          aes(x = ai_percent, y = profit,
                              color = `AI type`,
                              linetype = seeding_type,
                              group = interaction(`AI type`, seeding_type))) +
    stat_summary(fun = mean, geom = "line",size = 3) +
    stat_summary(fun.data = mean_cl_normal,geom = "errorbar", width = 0.05, linewidth = 2,linetype="solid") +
    labs(#title = paste0("Adoption rate at varing AI porpotion in the ",exp_label),
      x = "Proportion of artificial agents",
      y = "Adoption rate") +
    coord_cartesian(clip = "off")+
    scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0), breaks = seq(0.2, 1, 0.2))  +
    scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0), breaks = seq(0, 1, 0.2))+
    scale_color_manual(values=AI_colors, name="Random seeding",breaks = c( "Gemini", "Deepseek","GPT3.5","GPT5","Claude") )+
    scale_linetype_manual(name = NULL,  values = c("rand" = "solid", "degree" = "solid", "neigh_susc" = "solid", "low_th" = "solid"),
                          labels = c("rand" = "Random seeding", "degree" = "Degree seeding", "neigh_susc" = "Neighbor-susceptibility seeding", "low_th" = "Low-threshold seeding")) +
    guides(color = guide_legend(override.aes = list(linetype = "3313", linewidth = 2)),
           linetype = guide_legend(override.aes = list(linewidth = 2)) )+
    theme_paper(show_legend = TRUE)+
    theme(
      legend.box = "vertical",
      legend.direction = "vertical",
      legend.position="bottom right"
    )
  
  p_adoptionrate
  if (exp == "energy_policy") {
    p_adoptionrate <- p_adoptionrate + theme(axis.title.x = element_text(colour = "transparent"))
  }
  
  p_adoptionrate_degree <- p_adoptionrate %+% long_output[seeding_type == "degree", ]

  p_adoptionrate_random <- p_adoptionrate %+%  long_output[seeding_type == "rand", ]

  p_adoptionrate_neigh_susc <- p_adoptionrate %+% long_output[seeding_type == "neigh_susc", ]

  p_adoptionrate_low_th <- p_adoptionrate %+% long_output[seeding_type == "low_th", ]

  plot_store[[paste0("p_adoptionrate_", exp)]] <- p_adoptionrate
  plot_store[[paste0("p_adoptionrate_degree_", exp)]] <- p_adoptionrate_degree
  plot_store[[paste0("p_adoptionrate_random_", exp)]] <- p_adoptionrate_random
  plot_store[[paste0("p_adoptionrate_neigh_susc_", exp)]] <- p_adoptionrate_neigh_susc
  plot_store[[paste0("p_adoptionrate_low_th_", exp)]] <- p_adoptionrate_low_th

  ggsave(file.path(output_folder,paste0(exp,"_adoption rate n_adoption overall.png")), plot = p_adoptionrate, dpi = 100, width = 15, height = 12)
  ggsave(file.path(output_folder,paste0(exp,"_adoption rate n_adoption_degree.png")), plot = p_adoptionrate_degree, dpi = 100, width = 15, height = 12)
  ggsave(file.path(output_folder,paste0(exp,"_adoption rate n_adoption_random.png")), plot = p_adoptionrate_random, dpi = 100, width = 15, height = 12)
  ggsave(file.path(output_folder,paste0(exp,"_adoption rate n_adoption_neigh_susc.png")), plot = p_adoptionrate_neigh_susc, dpi = 100, width = 15, height = 12)
  ggsave(file.path(output_folder,paste0(exp,"_adoption rate n_adoption_low_th.png")), plot = p_adoptionrate_low_th, dpi = 100, width = 15, height = 12)
  
  #---------------- individual level: resistance ---------------------
  output <- foreach(sample_name = sample_name_list, .combine = "rbind") %do% {
    load(file.path(data_folder, "prepared_sample", sample_name))
    data = sample$coefs
    data.fplot <- GetDataFPlot(data, predictors = colnames(data)[!colnames(data) %in% c("resp.id")])
    data.fplot$sample_name <-  gsub(".rda","",sample_name) 
    # format sample name
    raw_name <- sub("\\.rda$", "", sample_name)
    data.fplot[, sample_name := format_sample_name(raw_name)]  # return this data.frame to combine in foreach
  }
  level_order <- c("Human", "GPT3.5", "Gemini", "GPT5","Deepseek", "Claude")
  levels_used <- intersect(level_order, unique(formatted_sample_names))
  output[, sample_name := factor(sample_name, levels = levels_used)]
  xcale_max<-max(output[,upper]) %>%ceiling()
  p_partworth <- ggplot(data = output, #output[!(variable %in% c("none","noneFALSE","noneTRUE")) ]
               aes(x = variable, y = mean, ymin = lower, ymax = upper, color = sample_name)) +
    geom_pointrange(position = position_dodge(width = 0.2), size = 0.001) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, position = position_dodge(.9)) +
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(NULL) +
    ylab("Average partworth utility (95% CI)") +
    facet_wrap(~sample_name, strip.position = "top", nrow = 1) + #scales = "free_x",
     scale_color_manual(values = sample_colors, breaks = levels(output$sample_name)) +
    theme_paper(base_size=35) +
    theme(legend.position = "none")
  if (exp == "energy_policy") {
    p_partworth <- p_partworth + theme(axis.title.x = element_text(colour = "transparent"))
  }
   plot_store[[paste0("p_partworth_", exp)]] <- p_partworth
  plot_store[[paste0("p_partworth_colors_", exp)]] <- sample_colors[levels(output$sample_name)]
  ggsave(file.path(output_folder,paste0(exp,"_partworth estimation.png")),
         plot = color_facet_strips(p_partworth, sample_colors[levels(output$sample_name)]),
         dpi = 100, width = 18, height = 12)
  
  
  return(plot_store)
}

#-----------------------------------------------------------
#main (baseline prompt): combined plot----------------------
#-----------------------------------------------------------
plot_store <- list()
for (cfg in exp_configs) {
  plot_store <- c(plot_store, make_plots(cfg,output_folder=output_folder))
}


# 6 main plots with legend removed
# p1, p2, p3, p4, p5, p6
p1<-plot_store[["p_vi_energy_policy"]]
p2<-plot_store[["p_t_energy_policy"]]
p3<-plot_store[["p_adoptionrate_degree_energy_policy"]]

p4<-plot_store[["p_vi_messaging_app"]]
p5<-plot_store[["p_t_messaging_app"]]
p6<-plot_store[["p_adoptionrate_degree_messaging_app"]]

#legend_grob 
legend_plot<-plot_store[["legend_plot_energy_policy"]]


tight_theme <- theme(
  plot.margin = margin(1, 1, 1, 1) # bottom, left, top, right
)

p1 <- p1 + tight_theme
p2 <- p2 + tight_theme
p3 <- p3 + tight_theme
p4 <- p4 + tight_theme
p5 <- p5 + tight_theme
p6 <- p6 + tight_theme
# 
main_panel <- plot_grid(
  p1, p2, p3,
  p4, p5, p6,
  ncol = 3,
  labels = c("A", "B", "C", "D", "E", "F"),
  label_x = 0.05,
  label_y = 1.12,
  label_size = 80,
  rel_widths = c(1.6, 1, 1),
  align = "hv",     #  horizontal / vertical
  axis = "tblr"   
  
)
# add while margin on th left and right
main_panel <- plot_grid(NULL, main_panel, NULL, ncol = 3, rel_widths = c(0.04, 1,0.02))

# add while margin on th top
main_panel <- plot_grid(NULL, main_panel, ncol = 1, rel_heights = c(0.08, 1))

main_panel <- ggdraw(main_panel) +
  draw_grob(
    rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),
    x = 0.005, y = 0.58, width = 0.03, height = 0.345
  ) +
  draw_grob(
    rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),
    x = 0.005, y = 0.118, width = 0.03, height = 0.345
  ) +
  draw_label("PS Experiment", x = 0.02, y = 0.755, angle = 90, fontface = "bold", size = 60) +
  draw_label("AA Experiment", x = 0.02, y = 0.295, angle = 90, fontface = "bold", size = 60)

# main_panel <- plot_grid(row1, row2, ncol = 1, align = "v")
final_plot <- plot_grid(main_panel, legend_plot, ncol = 1, rel_heights = c(1, 0.07))
ggsave(
  file.path(output_folder, paste0( "combined_plot.pdf")),
  final_plot,
  device = cairo_pdf,
  width = 42,
  height = 20,
  units = "in"
)

#------------------------------------------------------------------------------
# supplementary materials plot (baseline prompt):
# adoption rate across three seeding strategies
#------------------------------------------------------------------------------
qA <- plot_store[["p_adoptionrate_random_energy_policy"]]
qB <- plot_store[["p_adoptionrate_neigh_susc_energy_policy"]]
qC <- plot_store[["p_adoptionrate_low_th_energy_policy"]]
qD <- plot_store[["p_adoptionrate_random_messaging_app"]]
qE <- plot_store[["p_adoptionrate_neigh_susc_messaging_app"]]
qF <- plot_store[["p_adoptionrate_low_th_messaging_app"]]

tight_theme <- theme(
  plot.margin = margin(1, 1, 1, 1) # bottom, left, top, right
)

qA <- qA + tight_theme
qB <- qB + tight_theme + theme(axis.title.y = element_text(colour = "transparent"))
qC <- qC + tight_theme + theme(axis.title.y = element_text(colour = "transparent"))
qD <- qD + tight_theme
qE <- qE + tight_theme + theme(axis.title.y = element_text(colour = "transparent"))
qF <- qF + tight_theme + theme(axis.title.y = element_text(colour = "transparent"))

sup_seeding_panel <- plot_grid(
  qA, qB, qC,
  qD, qE, qF,
  ncol = 3,
  labels = c("A", "B", "C", "D", "E", "F"),
  label_x = 0.02,
  label_y = 1.18,
  label_size = 80,
  align = "hv",
  axis = "tblr"
)
# add white margin on the left and right
sup_seeding_panel <- plot_grid(NULL, sup_seeding_panel, NULL, ncol = 3, rel_widths = c(0.04, 1, 0.02))
sup_seeding_panel <- plot_grid(NULL, sup_seeding_panel, ncol = 1, rel_heights = c(0.16, 1))

sup_seeding_panel <- ggdraw(sup_seeding_panel) +
  draw_grob(
    rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),
    x = 0.005, y = 0.520, width = 0.03, height = 0.34
  ) +
  draw_grob(
    rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),
    x = 0.005, y = 0.090, width = 0.03, height = 0.34
  ) +
  draw_label("PS Experiment", x = 0.02, y = 0.693, angle = 90, fontface = "bold", size = 50) +
  draw_label("AA Experiment", x = 0.02, y = 0.265, angle = 90, fontface = "bold", size = 50) +
  draw_label("Random", x = 0.215, y = 0.93, fontface = "bold", size = 55) +
  draw_label("Neighbor-susceptibility", x = 0.529, y = 0.93, fontface = "bold", size = 55) +
  draw_label("Low-threshold", x = 0.844, y = 0.93, fontface = "bold", size = 55)

sup_seeding_panel <- plot_grid(sup_seeding_panel, legend_plot, ncol = 1, rel_heights = c(1, 0.07))
ggsave(
  file.path(output_folder, paste0("sup_adoptionrate_by_seeding.pdf")),
  sup_seeding_panel,
  device = cairo_pdf,
  width = 42,
  height = 20,
  units = "in"
)

#----------------------------------------------------------------
# supplementary materials (baseline prompt): plot-partworth------
#----------------------------------------------------------------
p9<-plot_store[["p_partworth_energy_policy"]]
p10 <- plot_store[["p_partworth_messaging_app"]]

tight_theme <- theme(
  plot.margin = margin(5, 5, 5, 5) # bottom, left, top, right
)

p9 <- p9+ tight_theme
p10 <- p10 + tight_theme
p9 <- color_facet_strips(p9, plot_store[["p_partworth_colors_energy_policy"]])
p10 <- color_facet_strips(p10, plot_store[["p_partworth_colors_messaging_app"]])
#
sup_panel <- plot_grid(
  p9,p10,
  ncol = 1,
  labels = c("A", "B"),
  label_x = 0.02,
  label_y = 1.03,
  label_size = 80,
  rel_widths = c(1),
  align = "hv",     #  horizontal / vertical
  axis = "tblr"    
)

# add while margin on th left and right
sup_panel <- plot_grid(NULL, sup_panel,ncol = 2, rel_widths = c(0.1, 1))
# add while margin on th top
sup_panel <- plot_grid(NULL, sup_panel, ncol = 1, rel_heights = c(0.05, 1))

sup_panel <- ggdraw(sup_panel) +
  draw_grob(
    rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),
    x = 0.02, y = 0.53, width = 0.06, height = 0.42
  ) +
  draw_grob(
    rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),
    x = 0.02, y = 0.05, width = 0.06, height = 0.42
  ) +
  draw_label("PS Experiment", x = 0.05, y = 0.74, angle = 90, fontface = "bold", size = 60) +
  draw_label("AA Experiment", x = 0.05, y = 0.26, angle = 90, fontface = "bold", size = 60)


ggsave(
  file.path(output_folder, paste0( "sup_partworth.pdf")),
  sup_panel,
  device = cairo_pdf,
  width = 28,
  height = 25,
  units = "in"
)

#-----------------------------------------------------------
# supplementary materials (baseline prompt): threshold distribution
#-----------------------------------------------------------
p11 <- plot_store[["p_t_dist_energy_policy"]]
p12 <- plot_store[["p_t_dist_messaging_app"]]

tight_theme <- theme(
  plot.margin = margin(5, 5, 5, 5) # bottom, left, top, right
)

p11 <- p11 + tight_theme
p12 <- p12 + tight_theme

sup_panel <- plot_grid(
  p11, p12,
  ncol = 1,
  labels = c("A", "B"),
  label_x = 0.02,
  label_y = 1.03,
  label_size = 80,
  rel_widths = c(1),
  align = "hv",     #  horizontal / vertical
  axis = "tblr"    
)

# add while margin on th left and right
sup_panel <- plot_grid(NULL, sup_panel, ncol = 2, rel_widths = c(0.1, 1))

# add while margin on th top
sup_panel <- plot_grid(NULL, sup_panel, ncol = 1, rel_heights = c(0.05, 1))

sup_panel <- ggdraw(sup_panel) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)), x = 0.01, y = 0.56, width = 0.08, height = 0.39 ) +
  draw_grob(rectGrob(gp = gpar(col = "black", fill = NA, lwd = 2)),  x = 0.01, y = 0.09, width = 0.08, height = 0.39) +
  draw_label("PS Experiment", x = 0.05, y = 0.75, angle = 90, fontface = "bold", size = 45) +
  draw_label("AA Experiment", x = 0.05, y = 0.28, angle = 90, fontface = "bold", size = 45)

ggsave(file.path(output_folder, paste0("sup_threshold_distribution.pdf")),sup_panel,device = cairo_pdf, width = 13,  height = 18,units = "in")
 

