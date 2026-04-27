test_that("gen_naturalearth_cover_share returns a raster", {
  skip_if_no_rawdata()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  res <- gen_naturalearth_cover_share()
  testthat::expect_s4_class(res, "SpatRaster")
})

test_that("gen_naturalearth_cover returns a raster with boolean values", {
  skip_if_no_rawdata()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  res <- gen_naturalearth_cover()
  testthat::expect_s4_class(res, "SpatRaster")
  testthat::expect_true(terra::is.bool(res))
})
