test_that(".pg_plot_palette maps colormap strings to distinct colour vectors", {
  expect_length(priogrid:::.pg_plot_palette("viridis", 4), 4)
  expect_false(identical(priogrid:::.pg_plot_palette("viridis", 8),
                         priogrid:::.pg_plot_palette("inferno", 8)))
})

test_that("plot_pgvariable draws a count variable from its metatags", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  r <- prio_blank_grid(cfg)
  terra::values(r) <- rep(c(0, 1, 5, NA), length.out = terra::ncell(r))
  names(r) <- "ucdp_ged"
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  save_pgvariable(r, "ucdp_ged", save_to = tmp)
  rr <- terra::rast(file.path(tmp, "cog", "ucdp_ged.tif"))

  png_path <- tempfile(fileext = ".png"); on.exit(unlink(png_path), add = TRUE)
  grDevices::png(png_path)
  expect_no_error(plot_pgvariable(rr))
  grDevices::dev.off()
  expect_true(file.exists(png_path) && file.info(png_path)$size > 0)
})

test_that("plot_pgvariable draws a discrete variable as classes", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  r <- prio_blank_grid(cfg)
  terra::values(r) <- rep(c(2, 20, 200, NA), length.out = terra::ncell(r))
  names(r) <- "cshapes_gwcode"
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  save_pgvariable(r, "cshapes_gwcode", save_to = tmp)
  rr <- terra::rast(file.path(tmp, "cog", "cshapes_gwcode.tif"))

  png_path <- tempfile(fileext = ".png"); on.exit(unlink(png_path), add = TRUE)
  grDevices::png(png_path)
  expect_no_error(plot_pgvariable(rr))
  grDevices::dev.off()
  expect_true(file.exists(png_path))
})
