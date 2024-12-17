pgvariables <- readr::read_tsv("data_raw/variables.csv")

usethis::use_data(pgvariables, overwrite = TRUE)
