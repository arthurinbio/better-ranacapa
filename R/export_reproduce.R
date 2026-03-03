build_reproduce_script <- function(input){
  # Minimal 这个可以自行调整
  lines <- c(
    "## better-renacapa reproduce script",
    "## This file captures key UI parameters (extend as needed).",
    "",
    paste0("topN_levels <- ", input$topN_levels),
    paste0("min_level_n <- ", input$min_level_n),
    paste0("color_by <- '", input$color_by, "'"),
    paste0("shape_by <- '", input$shape_by, "'"),
    "",
    "# TODO: Add your file paths and re-run filtering/ordination with the same parameters."
  )
  lines
}