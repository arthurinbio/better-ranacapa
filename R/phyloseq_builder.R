library(phyloseq)

build_phyloseq_from_tables <- function(tax, meta, sample_id_col){
  # Expect tax table: taxa as rows, samples as columns (common in ranacapa use)
  # First column might be taxon name; we try to detect.
  tax_df <- tax
  if (!all(meta[[sample_id_col]] %in% colnames(tax_df))){
    stop("Some SampleIDs in metadata are not present as columns in taxonomy table.")
  }

  # Use SampleIDs ordering from metadata
  sample_ids <- meta[[sample_id_col]]
  otu <- as.matrix(tax_df[, sample_ids, drop=FALSE])
  otu <- otu_table(otu, taxa_are_rows = TRUE)

  sd <- sample_data(meta)
  rownames(sd) <- meta[[sample_id_col]]

  phyloseq(otu, sd)
}