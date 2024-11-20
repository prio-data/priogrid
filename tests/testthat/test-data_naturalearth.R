test_that("gen_naturalearth_cover_share returns a raster", {
  res <- gen_naturalearth_cover_share()
  testthat::expect_s4_class(res, "SpatRaster")
})

test_that("gen_naturalearth_cover returns a raster with boolean values", {
  res <- gen_naturalearth_cover()
  testthat::expect_s4_class(res, "SpatRaster")
  testthat::expect_true(terra::is.bool(res))
})
