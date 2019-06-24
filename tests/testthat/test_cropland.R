context("Cropland data")

load("../../data_raw/cropland.rda")

test_that("data is RasterLayer", {
  expect_is(cropland, "RasterLayer")
})

test_that("resolution is 0.5X0.5", {
  expect_equal(res(cropland), rep(prio_resolution(), 2))
})

test_that("dimension is 360, 720, 1", {
  expect_equal(dim(cropland), c(prio_nrow(), prio_ncol(), 1))
})

test_that("dimension is 360, 720, 1", {
  expect_equal(dim(cropland), c(prio_nrow(), prio_ncol(), 1))
})

test_that("projection is correct", {
  expect_equal(proj4string(cropland), prio_crs())
})

test_that("extent is correct", {
  expect_equal( extent(cropland), prio_extent() )
})

test_that("little cropland in the middle of Sahara", {
  sahara <- SpatialPoints(data.frame(lat = 14.5, long = 22.5))
  expect_lt( raster::extract(cropland,  sahara), 0.05 )
  plot(cropland)
  plot(sahara, add = T)
})

test_that("much cropland in northern India", {
  uttar_pradesh <- SpatialPoints(data.frame(lat = 78.5, long = 29.5))
  expect_gt( raster::extract(cropland, uttar_pradesh ), 0.6 )
  plot(cropland)
  plot(uttar_pradesh, add = T)
})

# create explicit tests for cells where you have hand calculated values
