test_that("gen_cshapes_cover_share returns a raster", {
  res <- gen_cshapes_cover_share(as.Date("2010-01-01"))
  testthat::expect_s4_class(res, "SpatRaster")
})

test_that("gen_cshapes_cover_share works with measurement_dates outside cShapes data", {
  testthat::expect_warning(gen_cshapes_cover_share(as.Date("2024-01-01"))) |> # "[SpatVector from sf] empty SpatVector"
    testthat::expect_failure() # This should fail, as dates outside of cShapes should be handled.
})

test_that("gen_cshapes_cover returns a raster with boolean values", {
  res <- gen_cshapes_cover(as.Date("2010-01-01"))
  testthat::expect_s4_class(res, "SpatRaster")
  testthat::expect_true(terra::is.bool(res))
})
