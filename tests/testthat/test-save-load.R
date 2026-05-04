test_that("save_pgvariable and load_pgvariable round-trip works", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  varname <- pgvariables$name[1]

  r <- prio_blank_grid(cfg)
  terra::values(r) <- runif(terra::ncell(r))
  names(r) <- varname

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  save_pgvariable(r, varname, save_to = tmp)
  expect_true(file.exists(file.path(tmp, paste0(varname, ".rds"))))

  loaded <- terra::unwrap(readRDS(file.path(tmp, paste0(varname, ".rds"))))
  expect_s4_class(loaded, "SpatRaster")
  expect_equal(terra::values(loaded), terra::values(r))
})

test_that("save_pgvariable errors for unknown varname", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  r <- prio_blank_grid(cfg)
  expect_error(
    save_pgvariable(r, "not_a_real_variable", save_to = tempdir()),
    "not found in pgvariables"
  )
})
