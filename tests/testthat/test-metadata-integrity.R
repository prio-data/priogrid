test_that("every pgvariables name has a corresponding exported gen_ function", {
  exported <- getNamespaceExports("priogrid")
  missing <- pgvariables$name[!paste0("gen_", pgvariables$name) %in% exported]
  expect_length(missing, 0)
})

test_that("pgsources has no missing id, source_name, or source_version", {
  expect_false(any(is.na(pgsources$id)))
  expect_false(any(is.na(pgsources$source_name)))
  expect_false(any(is.na(pgsources$source_version)))
})

test_that("pgsources ids are unique", {
  expect_equal(length(pgsources$id), length(unique(pgsources$id)))
})

test_that("pgvariables has required columns", {
  expect_true(all(c("name", "static") %in% names(pgvariables)))
})

test_that("pgvariables names are unique", {
  expect_equal(nrow(pgvariables), length(unique(pgvariables$name)))
})

test_that("pgchecksum has required columns", {
  expect_true(all(c("source_name", "source_version", "id", "filename", "md5") %in% names(pgchecksum)))
})
