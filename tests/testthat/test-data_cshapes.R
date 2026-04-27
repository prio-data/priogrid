test_that("gen_cshapes_cover_share returns a multi-layer SpatRaster", {
  skip_if_no_rawdata()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  pg_set_config(start_date = as.Date("2010-12-31"), end_date = as.Date("2011-12-31"))
  on.exit(pg_reset_config(), add = TRUE)
  res <- gen_cshapes_cover_share()
  testthat::expect_s4_class(res, "SpatRaster")
  testthat::expect_gt(terra::nlyr(res), 0)
})

test_that("cshapes_cover_share returns a raster for a specific date", {
  skip_if_no_rawdata()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  res <- cshapes_cover_share(as.Date("2010-01-01"))
  testthat::expect_s4_class(res, "SpatRaster")
})

test_that("cshapes_cover returns a raster with boolean values", {
  skip_if_no_rawdata()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  res <- cshapes_cover(as.Date("2010-01-01"))
  testthat::expect_s4_class(res, "SpatRaster")
  testthat::expect_true(terra::is.bool(res))
})
