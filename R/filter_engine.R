library(shiny)

build_filter_ui <- function(meta, sample_id_col, session){
  cols <- setdiff(names(meta), sample_id_col)

  # Only build filters for columns with reasonable number of unique values
  tagList(
    lapply(cols, function(col){
      x <- meta[[col]]
      uniq <- unique(x)
      # numeric filter
      if (is.numeric(x)) {
        rng <- range(x, na.rm = TRUE)
        sliderInput(paste0("f_num_", col), col, min = rng[1], max = rng[2], value = rng)
      } else {
        # categorical filter with search-friendly selectize
        selectizeInput(
          paste0("f_cat_", col),
          label = col,
          choices = sort(na.omit(unique(as.character(x)))),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select levels (leave empty = no filter)")
        )
      }
    })
  )
}

apply_filters <- function(meta, sample_id_col, input){
  out <- meta

  for (col in setdiff(names(meta), sample_id_col)){
    num_id <- paste0("f_num_", col)
    cat_id <- paste0("f_cat_", col)

    if (!is.null(input[[num_id]]) && is.numeric(meta[[col]])){
      rng <- input[[num_id]]
      out <- out[!is.na(out[[col]]) & out[[col]] >= rng[1] & out[[col]] <= rng[2], , drop=FALSE]
    }

    if (!is.null(input[[cat_id]]) && !is.numeric(meta[[col]])){
      sel <- input[[cat_id]]
      if (length(sel) > 0){
        out <- out[as.character(out[[col]]) %in% sel, , drop=FALSE]
      }
    }
  }

  list(meta = out)
}