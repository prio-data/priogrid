pgsources <- readr::read_tsv("data_raw/sources.csv")

if(length(pgsources$id) != length(unique(pgsources$id))){
  stop("Non-unique IDs. Double check sources.csv")
}

usethis::use_data(pgsources, overwrite = TRUE)
