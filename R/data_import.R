read_table_auto <- function(path){
  # Guess delimiter by extension / content
  ext <- tools::file_ext(path)
  if (tolower(ext) == "csv") {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  }
}