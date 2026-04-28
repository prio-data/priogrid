library(lubridate)


test_that("prio-grid indices are from bottom-left to top-right", {
  cfg <- pg_config(ncol = 10L, nrow = 5L)
  expect_equal(create_pg_indices(cfg)[1,1], (10*5)-10+1)
  expect_equal(create_pg_indices(cfg)[5,1], 1)
  expect_equal(create_pg_indices(cfg)[5,10], 10)
  expect_equal(create_pg_indices(cfg)[1,10], 10*5)
})

test_that("prio-grid raster parameters are correct", {
  skip_if_not_installed("terra")
  cfg <- pg_current_config()
  pg <- prio_blank_grid(cfg)
  expect_equal(terra::ext(pg) |> as.vector(), cfg$extent)
  expect_equal(terra::crs(pg), terra::crs(cfg$crs))
  expect_equal(terra::ncol(pg), cfg$ncol)
  expect_equal(terra::nrow(pg), cfg$nrow)
})

# Test suite for pg_dates function
test_that("pg_dates handles basic monthly sequences", {
  cfg <- pg_config(
    start_date = as.Date("2023-01-15"),
    end_date = as.Date("2023-03-18"),
    temporal_resolution = "1 month"
  )
  result <- pg_dates(cfg)

  expected <- c(as.Date("2023-01-15"), as.Date("2023-02-15"), as.Date("2023-03-15"))

  expect_equal(result, expected)
})

test_that("pg_dates handles end-of-month dates at monthly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2023-01-31"),
    end_date = as.Date("2023-04-18"),
    temporal_resolution = "1 month"
  )
  result <- pg_dates(cfg)

  expected <- c(as.Date("2023-01-31"),
                as.Date("2023-02-28"),
                as.Date("2023-03-31"))

  expect_equal(result, expected)
})

test_that("pg_dates handles end-of-month dates at quarterly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2023-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 quarter"
  )
  result <- pg_dates(cfg)

  expected <- c(as.Date("2023-01-31"),
                as.Date("2023-04-30"),
                as.Date("2023-07-31"),
                as.Date("2023-10-31"))

  expect_equal(result, expected)
})

test_that("pg_dates handles end-of-month dates at yearly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2020-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 year"
  )
  result <- pg_dates(cfg)

  expected <- c(as.Date("2020-01-31"),
                as.Date("2021-01-31"),
                as.Date("2022-01-31"),
                as.Date("2023-01-31"))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles end-of-month dates at yearly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2020-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 year"
  )
  result <- pg_date_intervals(cfg)

  expected <- c(lubridate::interval(as.Date("2019-02-01"), as.Date("2020-01-31")),
                lubridate::interval(as.Date("2020-02-01"), as.Date("2021-01-31")),
                lubridate::interval(as.Date("2021-02-01"), as.Date("2022-01-31")),
                lubridate::interval(as.Date("2022-02-01"), as.Date("2023-01-31")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles end-of-month dates at monthly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2020-01-31"),
    end_date = as.Date("2020-04-18"),
    temporal_resolution = "1 month"
  )
  result <- pg_date_intervals(cfg)

  expected <- c(lubridate::interval(as.Date("2020-01-01"), as.Date("2020-01-31")),
                lubridate::interval(as.Date("2020-02-01"), as.Date("2020-02-29")),
                lubridate::interval(as.Date("2020-03-01"), as.Date("2020-03-31")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles dates at monthly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2020-01-15"),
    end_date = as.Date("2020-04-18"),
    temporal_resolution = "1 month"
  )
  result <- pg_date_intervals(cfg)

  expected <- c(lubridate::interval(as.Date("2019-12-16"), as.Date("2020-01-15")),
                lubridate::interval(as.Date("2020-01-16"), as.Date("2020-02-15")),
                lubridate::interval(as.Date("2020-02-16"), as.Date("2020-03-15")),
                lubridate::interval(as.Date("2020-03-16"), as.Date("2020-04-15")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles dates at quarterly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2023-01-15"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 quarter"
  )
  result <- pg_date_intervals(cfg)

  expected <- c(lubridate::interval(as.Date("2022-10-16"), as.Date("2023-01-15")),
                lubridate::interval(as.Date("2023-01-16"), as.Date("2023-04-15")),
                lubridate::interval(as.Date("2023-04-16"), as.Date("2023-07-15")),
                lubridate::interval(as.Date("2023-07-16"), as.Date("2023-10-15")),
                lubridate::interval(as.Date("2023-10-16"), as.Date("2024-01-15")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles end-of-month dates at quarterly resolution", {
  cfg <- pg_config(
    start_date = as.Date("2023-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 quarter"
  )
  result <- pg_date_intervals(cfg)

  expected <- c(lubridate::interval(as.Date("2022-11-01"), as.Date("2023-01-31")),
                lubridate::interval(as.Date("2023-02-01"), as.Date("2023-04-30")),
                lubridate::interval(as.Date("2023-05-01"), as.Date("2023-07-31")),
                lubridate::interval(as.Date("2023-08-01"), as.Date("2023-10-31")))

  expect_equal(result, expected)
})

test_that("rast_to_df converts static raster to data.table with pgid column", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  r <- prio_blank_grid(cfg)
  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "myvar"
  df <- rast_to_df(r, static = TRUE, config = cfg)
  expect_true("pgid" %in% names(df))
  expect_true("myvar" %in% names(df))
  expect_true(nrow(df) > 0)
})

test_that("rast_to_df converts time-varying raster with measurement_date column", {
  skip_if_not_installed("terra")
  cfg <- test_config()
  dates <- pg_dates(cfg)
  r <- prio_blank_grid(cfg)
  for (i in seq_along(dates)[-1]) terra::add(r) <- prio_blank_grid(cfg)
  terra::values(r) <- seq_len(terra::ncell(r) * terra::nlyr(r))
  names(r) <- as.character(dates)
  df <- rast_to_df(r, static = FALSE, varname = "myvar", config = cfg)
  expect_true("measurement_date" %in% names(df))
  expect_true("myvar" %in% names(df))
  expect_true("pgid" %in% names(df))
})
