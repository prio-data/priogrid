# download_file_httr2 was made internal in the refactor.
# Download behaviour is covered indirectly by integration tests that call
# get_pgfile() / download_pg_rawdata() when raw data is present.
test_that("pg_data_availability returns NULL gracefully when rawfolder is unset", {
  skip_if(tryCatch({pg_rawfolder(); TRUE}, error = function(e) FALSE),
          "rawfolder already set in this environment")
  expect_null(pg_data_availability())
})

test_that("pg_data_availability returns a data frame when rawfolder is set", {
  skip_if_no_rawdata()
  res <- pg_data_availability()
  expect_s3_class(res, "data.frame")
  expect_true(all(c("source_name", "source_version", "n_files", "n_present", "all_present") %in% names(res)))
  expect_gt(nrow(res), 0)
})
