# Skip test if raw data folder is unset
skip_if_no_rawdata <- function() {
  tryCatch(pg_rawfolder(), error = function(e) testthat::skip("Raw data folder not set"))
}

# Tiny config for fast tests that need no downloaded data
test_config <- function() {
  pg_config(nrow = 5L, ncol = 10L,
            start_date = as.Date("2010-12-31"),
            end_date   = as.Date("2012-12-31"),
            temporal_resolution = "1 year")
}
