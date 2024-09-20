test_that("prio-grid indicies are from bottom-left to top-right", {
  ncol <- 10
  nrow <- 5
  expect_equal(create_pg_indices(ncol, nrow)[1,1], (ncol*nrow)-ncol+1)
  expect_equal(create_pg_indices(ncol, nrow)[nrow,1], 1)
  expect_equal(create_pg_indices(ncol, nrow)[nrow,ncol], ncol)
  expect_equal(create_pg_indices(ncol, nrow)[1,ncol], ncol*nrow)
})

test_that("prio-grid raster parameters are correct", {
  expect_equal(terra::ext(prio_blank_grid()) |> as.vector(), pgoptions$get_extent())
  expect_equal(terra::crs(prio_blank_grid()), terra::crs(pgoptions$get_crs()))
  expect_equal(terra::ncol(prio_blank_grid()), pgoptions$get_ncol())
  expect_equal(terra::nrow(prio_blank_grid()), pgoptions$get_nrow())
})

test_that("raster_to_pgtibble", {
  pg <- prio_blank_grid()
  names(pg) <- "test"
  expect_s3_class(raster_to_pgtibble(pg), "tbl_df")
})
