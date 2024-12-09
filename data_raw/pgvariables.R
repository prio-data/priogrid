pgvariables <- readr::read_csv2("data_raw/variables.csv")

usethis::use_data(pgvariables, overwrite = FALSE)
