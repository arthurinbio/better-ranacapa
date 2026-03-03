library(phyloseq)
library(ggplot2)

collapse_levels <- function(vec, topN_levels = 12, min_level_n = 5){
  v <- as.character(vec)
  tab <- sort(table(v), decreasing = TRUE)
  keep <- names(tab)[tab >= min_level_n]
  keep <- intersect(keep, names(tab)[1:min(topN_levels, length(tab))])
  v[!(v %in% keep)] <- "Other"
  factor(v)
}

make_ordination_plot <- function(ps, color_by="None", shape_by="None",
                                 topN_levels=12, min_level_n=5){
  ord <- ordinate(ps, method = "PCoA", distance = "bray")
  df <- as.data.frame(ord$vectors[,1:2])
  df$SampleID <- rownames(df)
  meta <- data.frame(sample_data(ps))
  meta$SampleID <- rownames(meta)
  df <- merge(df, meta, by="SampleID", all.x=TRUE)

  aes_map <- aes(x = Axis.1, y = Axis.2, text = paste("Sample:", SampleID))

  if (!is.null(color_by) && color_by != "None" && color_by %in% names(df)){
    df[[color_by]] <- collapse_levels(df[[color_by]], topN_levels, min_level_n)
    aes_map <- modifyList(aes_map, aes(color = .data[[color_by]]))
  }
  if (!is.null(shape_by) && shape_by != "None" && shape_by %in% names(df)){
    df[[shape_by]] <- collapse_levels(df[[shape_by]], topN_levels, min_level_n)
    aes_map <- modifyList(aes_map, aes(shape = .data[[shape_by]]))
  }

  ggplot(df, aes_map) +
    geom_point(size = 2.5, alpha = 0.9) +
    theme_minimal() +
    labs(x = "PCoA 1", y = "PCoA 2")
}