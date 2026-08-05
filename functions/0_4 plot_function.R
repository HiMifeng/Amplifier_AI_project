# Description: Shared plotting helpers - variable importance, partworth data, sample-name formatting, ggplot theme.
# Input: fitted coefs / attribute data.table
# Output: data.table / ggplot theme object

GetVariableImportance <- function(fit.hb, attributes){
  # Compute average utilities by resp
  fit.hb <- fit.hb[, lapply(.SD, mean), by=resp.id]
  # Remove none option from the attributes
  attributes <- attributes[value!=0]
  # Get the variable name in fit.hb
  attributes[, variable:=paste(attribute, value, sep="")]
  res.range <- foreach(i = unique(attributes$attribute), .combine = "cbind") %do% {
    if(i == "social_signal"){
      utility.range = (max(as.numeric(attributes[attribute=="social_signal"]$value)) -
                         min(as.numeric(attributes[attribute=="social_signal"]$value))) * fit.hb$social_signal
    } else {
      # Subset data for a given attribute
      tmp <- attributes[attribute==i]
      # Calculate the range in utilities
      utility.range <- apply(fit.hb[, tmp$variable, with=FALSE], 1, function(x) (max(x) - min(x)))
    }
    # Return
    utility.range
  }
  colnames(res.range) <- unique(attributes$attribute)

  # Calculate importance: Normalise range per respondent
  res <- res.range/apply(res.range, 1, sum)
  n <- nrow(res)
  ci_factor <- 1.96 / sqrt(n)
  # Get mean and sd over respondents
  mean_importance = apply(res, 2, mean); sd_importance = apply(res, 2, sd)
  se_importance = sd_importance/sqrt(n)
  upper_bound <- mean_importance + ci_factor * sd_importance
  lower_bound <- mean_importance - ci_factor * sd_importance
  # return
  importance_df <- data.table(
    variable = colnames(res),
    mean=mean_importance,
    sd=sd_importance,
    se=se_importance,
    upper=upper_bound,
    lower=lower_bound)

  return(importance_df)
}

GetDataFPlot <- function(fit.hb, predictors){
  # Comoute average utilities by resp
  fit.hb <- fit.hb[, lapply(.SD, mean), by=resp.id]
  f.mean <- apply(fit.hb[, predictors, with=FALSE], 2, mean) # Mean
  f.sd <- apply(fit.hb[, predictors, with=FALSE], 2, sd)  # SD
  f.upper <- f.mean + qnorm(0.975)*f.sd/sqrt(nrow(fit.hb)) # 95% CI upper bound
  f.lower <- f.mean - qnorm(0.975)*f.sd/sqrt(nrow(fit.hb)) # 95% CI lower bound
  data.table(variable = factor(predictors), mean = f.mean, upper = f.upper, lower = f.lower)
}

format_sample_name <- function(x) {
  data.table::fcase(
    grepl("real", x, ignore.case = TRUE), "Human",
    grepl("gemini", x, ignore.case = TRUE), "Gemini",
    grepl("claude", x, ignore.case = TRUE), "Claude",
    grepl("deepseek", x, ignore.case = TRUE), "Deepseek",
    grepl("gpt3\\.5", x, ignore.case = TRUE), "GPT3.5",
    grepl("gpt5", x, ignore.case = TRUE), "GPT5",
    default = x
  )
}

# Order a formatted sample-name vector (as produced by format_sample_name()) into the
# project-wide agent display order: Human, GPT3.5, Gemini, GPT5, Deepseek, Claude. Used to build
# the `levels=` argument for factor(sample_name), so downstream fill/facet ordering is
# consistent across "6 plot.R"'s importance/threshold/partworth plots (extracted from 3
# byte-identical copies of this block).
sample_name_levels <- function(x) {
  c(
    grep("Human", unique(x), ignore.case = TRUE, value = TRUE),
    grep("GPT3.5", unique(x), ignore.case = TRUE, value = TRUE),
    grep("Gemini", unique(x), ignore.case = TRUE, value = TRUE),
    grep("GPT5", unique(x), ignore.case = TRUE, value = TRUE),
    grep("Deepseek", unique(x), ignore.case = TRUE, value = TRUE),
    grep("Claude", unique(x), ignore.case = TRUE, value = TRUE)
  )
}

# Tidy an importance data.table's `variable` column for display: title-case, underscores to
# spaces, then two specific renames ("Policytype" -> "Policy type", "Org" -> "Organization").
# Used identically by both "6 plot.R" and "6 plot SM.R"'s variable-importance plots. Modifies
# `dt` in place (data.table semantics) and returns it invisibly, matching how the original
# inline block was used inside a foreach loop body.
format_importance_variable_names <- function(dt) {
  dt[, variable := tools::toTitleCase(variable)]
  dt[, variable := gsub("_", " ", variable)]
  dt[variable == "Policytype", variable := "Policy type"]
  dt[variable == "Org", variable := "Organization"]
  invisible(dt)
}

# The shared part of the bottom-legend theme used by "6 plot.R"'s p_t_legend and
# "6 plot SM.R"'s p_legend_sm (each then adds its own guides() on top, which differ slightly
# between the two, so those are left at the call site).
theme_legend_bottom <- function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(1.5, "cm"),
    legend.spacing.x = unit(2, "cm")
  )
}

theme_paper <- function(base_size = 45, show_legend = FALSE) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.ticks = element_line(color = "black"),

      axis.title.x = element_text(size = base_size),
      axis.title.y = element_text(size = base_size),
      axis.text.x = element_text(size = base_size),
      axis.text.y = element_text(size = base_size),

      legend.position = if (show_legend) "right" else "none",
      legend.background = element_rect(fill = "white", colour = "black", linewidth = 0.8),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.width = unit(2, "cm"),
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size),

      plot.title = element_text(size = base_size , hjust = 0.5)
    )
}

# Recolor each facet_wrap strip's background to match that panel's color. ggplot2 has no
# built-in support for per-facet strip colors (that requires the ggh4x package); this instead
# edits the rendered ggplotGrob directly, so no new dependency is needed. `colors` must be a
# vector in the same left-to-right order as the plot's facet panels (i.e. indexed by the
# faceting variable's factor level order). Returns the gtable itself (not wrapped in
# cowplot::ggdraw()) so that plot_grid(..., align = "hv", axis = "tblr") can still introspect
# the original panel grobs to align it against other plots -- wrapping in ggdraw() turns it
# into an opaque canvas that breaks that alignment. Both ggsave() and plot_grid() accept a
# gtable directly.
color_facet_strips <- function(plot, colors, alpha = 1) {
  g <- ggplotGrob(plot)
  strip_idx <- which(grepl("^strip-t", g$layout$name))
  stopifnot(length(strip_idx) == length(colors))
  fill_colors <- scales::alpha(colors, alpha)
  for (k in seq_along(strip_idx)) {
    i <- strip_idx[k]
    rect_pos <- which(vapply(g$grobs[[i]]$grobs[[1]]$children, inherits, logical(1), "rect"))
    g$grobs[[i]]$grobs[[1]]$children[[rect_pos]]$gp$fill <- fill_colors[k]
  }
  g
}
