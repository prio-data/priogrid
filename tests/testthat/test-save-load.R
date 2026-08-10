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
  expect_true(file.exists(file.path(tmp, "cog", paste0(varname, ".tif"))))

  loaded <- terra::rast(file.path(tmp, "cog", paste0(varname, ".tif")))
  expect_s4_class(loaded, "SpatRaster")
  expect_equal(terra::values(loaded), terra::values(r), tolerance = 1e-5)
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


test_that("save_pgvariable stamps plotting metatags for a discrete variable", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  varname <- "cshapes_gwcode"                 # plot_type = discrete
  r <- prio_blank_grid(cfg)
  terra::values(r) <- rep(c(2, 20, 20, 200, NA), length.out = terra::ncell(r))
  names(r) <- varname

  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  save_pgvariable(r, varname, save_to = tmp)

  tags <- terra::metags(terra::rast(file.path(tmp, "cog", paste0(varname, ".tif"))))
  get  <- function(k) tags$value[tags$name == k]
  expect_equal(get("pg_plot_type"), "discrete")
  expect_equal(get("pg_colormap"), "tab20")
  expect_equal(as.numeric(get("pg_value_min")), 2)
  expect_equal(as.numeric(get("pg_value_max")), 200)
  expect_equal(as.integer(get("pg_nunique")), 3L)      # {2, 20, 200}
  expect_equal(get("pg_class_values"), "2,20,200")
  expect_true("pg_value_mean" %in% tags$name)
  expect_false("pg_palette" %in% tags$name)
})

test_that("save_pgvariable derives colormap/transform for a count variable", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  varname <- "ucdp_ged"                        # plot_type = count, transform = log1p
  r <- prio_blank_grid(cfg)
  terra::values(r) <- rep(c(0, 1, 5, NA), length.out = terra::ncell(r))
  names(r) <- varname

  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  save_pgvariable(r, varname, save_to = tmp)

  tags <- terra::metags(terra::rast(file.path(tmp, "cog", paste0(varname, ".tif"))))
  get  <- function(k) tags$value[tags$name == k]
  expect_equal(get("pg_transform"), "log1p")
  expect_equal(get("pg_colormap"), "inferno")
  expect_false("pg_class_values" %in% tags$name)   # not discrete
})