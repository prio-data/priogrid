test_that("pgsearch returns a list with expected named elements", {
  res <- pgsearch("GHSL")
  expect_type(res, "list")
  expect_true(all(c("in_name", "in_version", "in_id", "in_tags",
                    "in_spatial_extent", "in_temporal_resolution") %in% names(res)))
})

test_that("pgsearch finds known sources by name", {
  res <- pgsearch("GHSL")
  expect_gt(nrow(res$in_name), 0)
})

test_that("pgsearch returns empty data frames for unknown string", {
  res <- pgsearch("XYZNONEXISTENT999")
  total_rows <- sum(sapply(res, nrow))
  expect_equal(total_rows, 0)
})

test_that("pgsearch is case-insensitive", {
  res_upper <- pgsearch("UCDP")
  res_lower <- pgsearch("ucdp")
  expect_equal(nrow(res_upper$in_name), nrow(res_lower$in_name))
})

test_that("pg_rawfiles returns expected columns", {
  fi <- pg_rawfiles()
  expect_true(all(c("source_name", "source_version", "id", "url", "filename") %in% names(fi)))
  expect_gt(nrow(fi), 0)
})

test_that("pg_rawfiles only_file_extensions returns character vector", {
  exts <- pg_rawfiles(only_file_extensions = TRUE)
  expect_type(exts, "character")
  expect_gt(length(exts), 0)
})
