# Description: Patch ChoiceModelR to keep never-chosen alternatives (incl. none) in the choice table.
# Input: none (patches the installed ChoiceModelR package at runtime)
# Output: none (side effect only)

library(data.table)
library(ggplot2)
library(igraph)
library(scales)

# patches ChoiceModelR::choicemodelr() to correctly handle alternatives, 
# including the none option, that were never selected.
patch_ChoiceModelR_none <- function() {
  
  # Retrieve the original choicemodelr function
  f <- ChoiceModelR:::choicemodelr
  
  # Convert the function body to text
  code <- paste(
    deparse(body(f), width.cutoff = 500),
    collapse = "\n"
  )

  # assignInNamespace() below persists for the life of the R session, so re-sourcing this
  # script (without restarting R) would otherwise see the already-patched body and fail the
  # table(y) check. Detect that case and skip instead of erroring.
  if (grepl("table\\(factor\\(y, levels = seq_len\\(maxalts\\)\\)\\)", code)) {
    message("ChoiceModelR none-option patch already applied, skipping.")
    return(invisible(NULL))
  }

  # Check whether the expected table(y) expression exists
  if (!grepl("table\\(y\\)", code)) {
    stop(
      "Could not find table(y). The installed ChoiceModelR version may be different."
    )
  }
  # Preserve alternatives that were never selected by explicitly
  # including all alternative levels, including the none option
  code <- sub(
    "table\\(y\\)",
    "table(factor(y, levels = seq_len(maxalts)))",
    code
  )
  
  # Replace the original function body with the patched version
  body(f) <- parse(text = code)[[1]]
  # Replace choicemodelr in the ChoiceModelR namespace
  assignInNamespace(
    "choicemodelr",
    f,
    ns = "ChoiceModelR"
  )
  
  message(
    "ChoiceModelR none-option patch applied successfully."
  )
}
