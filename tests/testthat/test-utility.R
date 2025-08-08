library(lubridate)


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

# Test suite for pg_dates function
test_that("pg_dates handles basic monthly sequences", {
  # Test basic monthly sequence
  result <- pg_dates(
    start_date = as.Date("2023-01-15"),
    end_date = as.Date("2023-03-18"),
    temporal_resolution = "1 month"
  )

  expected <- c(as.Date("2023-01-15"), as.Date("2023-02-15"), as.Date("2023-03-15"))

  expect_equal(result, expected)
})

test_that("pg_dates handles end-of-month dates at monthly resolution", {
  # Test basic monthly sequence
  result <- pg_dates(
    start_date = as.Date("2023-01-31"),
    end_date = as.Date("2023-04-18"),
    temporal_resolution = "1 month"
  )

  expected <- c(as.Date("2023-01-31"),
                as.Date("2023-02-28"),
                as.Date("2023-03-31"))

  expect_equal(result, expected)
})

test_that("pg_dates handles end-of-month dates at quarterly resolution", {
  # Test basic monthly sequence
  result <- pg_dates(
    start_date = as.Date("2023-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 quarter"
  )

  expected <- c(as.Date("2023-01-31"),
                as.Date("2023-04-30"),
                as.Date("2023-07-31"),
                as.Date("2023-10-31"))

  expect_equal(result, expected)
})

test_that("pg_dates handles end-of-month dates at yearly resolution", {
  # Test basic monthly sequence
  result <- pg_dates(
    start_date = as.Date("2020-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 year"
  )

  expected <- c(as.Date("2020-01-31"),
                as.Date("2021-01-31"),
                as.Date("2022-01-31"),
                as.Date("2023-01-31"))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles end-of-month dates at yearly resolution", {
  # Test basic monthly sequence
  result <- pg_date_intervals(
    start_date = as.Date("2020-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 year"
  )

  expected <- c(lubridate::interval(as.Date("2019-02-01"), as.Date("2020-01-31")),
                lubridate::interval(as.Date("2020-02-01"), as.Date("2021-01-31")),
                lubridate::interval(as.Date("2021-02-01"), as.Date("2022-01-31")),
                lubridate::interval(as.Date("2022-02-01"), as.Date("2023-01-31")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles end-of-month dates at monthly resolution", {
  # Test basic monthly sequence
  result <- pg_date_intervals(
    start_date = as.Date("2020-01-31"),
    end_date = as.Date("2020-04-18"),
    temporal_resolution = "1 month"
  )

  expected <- c(lubridate::interval(as.Date("2020-01-01"), as.Date("2020-01-31")),
                lubridate::interval(as.Date("2020-02-01"), as.Date("2020-02-29")),
                lubridate::interval(as.Date("2020-03-01"), as.Date("2020-03-31")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles dates at monthly resolution", {
  # Test basic monthly sequence
  result <- pg_date_intervals(
    start_date = as.Date("2020-01-15"),
    end_date = as.Date("2020-04-18"),
    temporal_resolution = "1 month"
  )

  expected <- c(lubridate::interval(as.Date("2019-12-16"), as.Date("2020-01-15")),
                lubridate::interval(as.Date("2020-01-16"), as.Date("2020-02-15")),
                lubridate::interval(as.Date("2020-02-16"), as.Date("2020-03-15")),
                lubridate::interval(as.Date("2020-03-16"), as.Date("2020-04-15")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles dates at quarterly resolution", {
  # Test basic monthly sequence
  result <- pg_date_intervals(
    start_date = as.Date("2023-01-15"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 quarter"
  )

  expected <- c(lubridate::interval(as.Date("2022-10-16"), as.Date("2023-01-15")),
                lubridate::interval(as.Date("2023-01-16"), as.Date("2023-04-15")),
                lubridate::interval(as.Date("2023-04-16"), as.Date("2023-07-15")),
                lubridate::interval(as.Date("2023-07-16"), as.Date("2023-10-15")),
                lubridate::interval(as.Date("2023-10-16"), as.Date("2024-01-15")))

  expect_equal(result, expected)
})

test_that("pg_date_intervals handles end-of-month dates at quarterly resolution", {
  # Test basic monthly sequence
  result <- pg_date_intervals(
    start_date = as.Date("2023-01-31"),
    end_date = as.Date("2024-01-18"),
    temporal_resolution = "1 quarter"
  )

  expected <- c(lubridate::interval(as.Date("2022-11-01"), as.Date("2023-01-31")),
                lubridate::interval(as.Date("2023-02-01"), as.Date("2023-04-30")),
                lubridate::interval(as.Date("2023-05-01"), as.Date("2023-07-31")),
                lubridate::interval(as.Date("2023-08-01"), as.Date("2023-10-31")))

  expect_equal(result, expected)
})
