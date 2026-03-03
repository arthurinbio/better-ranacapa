library(data.table)

profile_metadata <- function(meta, sample_id_col){
  stopifnot(sample_id_col %in% names(meta))
  dt <- as.data.table(meta)

  prof <- lapply(names(dt), function(col){
    x <- dt[[col]]
    miss <- mean(is.na(x) | x == "")
    uniq <- length(unique(x))
    typ <- if (col == sample_id_col) "sample_id" else if (is.numeric(x)) "numeric" else "categorical"
    top <- ""
    if (typ == "categorical"){
      tab <- sort(table(x), decreasing = TRUE)
      top <- paste0(names(tab)[1:min(5, length(tab))], " (", tab[1:min(5, length(tab))], ")", collapse = "; ")
    }
    data.frame(
      column = col,
      type = typ,
      unique_values = uniq,
      missing_rate = round(miss, 4),
      top_levels = top,
      stringsAsFactors = FALSE
    )
  })

  rbindlist(prof)
}