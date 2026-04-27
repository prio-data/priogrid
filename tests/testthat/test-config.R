test_that("pg_config creates object with correct defaults", {
  cfg <- pg_config()
  expect_s3_class(cfg, "pg_config")
  expect_equal(cfg$nrow, 360L)
  expect_equal(cfg$ncol, 720L)
  expect_equal(cfg$crs, "epsg:4326")
  expect_equal(cfg$extent, c(xmin = -180, xmax = 180, ymin = -90, ymax = 90))
  expect_equal(cfg$temporal_resolution, "1 year")
  expect_equal(cfg$start_date, as.Date("1850-12-31"))
  expect_true(cfg$verbose)
  expect_true(cfg$automatic_download)
})

test_that("pg_config validates inputs", {
  expect_error(pg_config(nrow = -1), "positive integer")
  expect_error(pg_config(ncol = 0), "positive integer")
  expect_error(pg_config(extent = c(1, 2)), "length 4")
  expect_error(pg_config(verbose = "yes"), "logical") # as.logical("yes") = NA, which is caught
})

test_that("pg_config extent always has canonical names", {
  cfg_named   <- pg_config(extent = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90))
  cfg_unnamed <- pg_config(extent = c(-180, 180, -90, 90))
  expect_equal(names(cfg_named$extent),   c("xmin", "xmax", "ymin", "ymax"))
  expect_equal(names(cfg_unnamed$extent), c("xmin", "xmax", "ymin", "ymax"))
})

test_that("pg_config accepts custom parameters", {
  cfg <- pg_config(nrow = 180L, ncol = 360L, crs = "epsg:3857")
  expect_equal(cfg$nrow, 180L)
  expect_equal(cfg$ncol, 360L)
  expect_equal(cfg$crs, "epsg:3857")
})

test_that("pg_current_config initializes lazily", {
  pg_reset_config()
  cfg <- pg_current_config()
  expect_s3_class(cfg, "pg_config")
  expect_equal(cfg$nrow, 360L)
})

test_that("pg_set_config updates session state", {
  pg_reset_config()
  pg_set_config(nrow = 180L)
  expect_equal(pg_current_config()$nrow, 180L)
  # Other fields unchanged
  expect_equal(pg_current_config()$ncol, 720L)
  pg_reset_config()
})

test_that("pg_set_config rejects invalid fields", {
  expect_error(pg_set_config(invalid_field = 42), "Unknown config fields")
})

test_that("pg_reset_config restores defaults", {
  pg_set_config(nrow = 180L, ncol = 360L)
  pg_reset_config()
  expect_equal(pg_current_config()$nrow, 360L)
  expect_equal(pg_current_config()$ncol, 720L)
})

test_that("pg_rawfolder errors when unset", {
  # This test relies on the cache not having rawfolder set,
  # which may not be true in all environments. Skip if already set.
  skip_if(tryCatch({pg_rawfolder(); TRUE}, error = function(e) FALSE),
          "rawfolder already set in this environment")
  expect_error(pg_rawfolder(), "not set")
})

test_that("print.pg_config works", {
  cfg <- pg_config()
  expect_output(print(cfg), "PRIO-GRID config")
  expect_output(print(cfg), "nrow: 360")
})
