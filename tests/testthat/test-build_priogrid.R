test_that("resolve_pg_mode defaults to release mode", {
  cfg <- resolve_pg_mode(config = pg_config())
  expect_equal(cfg$mode, "release")
  expect_equal(cfg$type, "05deg_yearly")
  expect_false(cfg$overwrite)
})

test_that("resolve_pg_mode with overwrite=TRUE defaults to custom mode", {
  cfg <- resolve_pg_mode(overwrite = TRUE, config = pg_config())
  expect_equal(cfg$mode, "custom")
  expect_true(cfg$overwrite)
})

test_that("resolve_pg_mode warns and ignores overwrite for releases", {
  cfg <- resolve_pg_mode(version = "3.0.0", overwrite = TRUE, config = pg_config())
  expect_equal(cfg$mode, "release")
  expect_false(cfg$overwrite)
  expect_false(is.null(cfg$warning))
})

test_that("resolve_pg_mode errors on mixed params", {
  expect_error(
    resolve_pg_mode(version = "3.0.0", spatial_hash = "abc123", config = pg_config()),
    "Cannot mix"
  )
})

test_that("resolve_pg_mode errors on partial hash params", {
  expect_error(
    resolve_pg_mode(spatial_hash = "abc123", config = pg_config()),
    "Both spatial_hash and temporal_hash"
  )
})

test_that("get_spatial_hash returns consistent 6-character string", {
  cfg <- pg_config()
  h1 <- priogrid:::get_spatial_hash(cfg)
  h2 <- priogrid:::get_spatial_hash(cfg)
  expect_equal(h1, h2)
  expect_equal(nchar(h1), 6)
})

test_that("get_spatial_hash differs for different configs", {
  h1 <- priogrid:::get_spatial_hash(pg_config(nrow = 360L, ncol = 720L))
  h2 <- priogrid:::get_spatial_hash(pg_config(nrow = 180L, ncol = 360L))
  expect_false(h1 == h2)
})

test_that("get_temporal_hash returns consistent 6-character string", {
  cfg <- pg_config()
  h <- priogrid:::get_temporal_hash(cfg)
  expect_equal(nchar(h), 6)
  expect_equal(h, priogrid:::get_temporal_hash(cfg))
})

test_that("get_temporal_hash ignores small end_date changes within same period", {
  cfg1 <- pg_config(end_date = as.Date("2024-06-15"), temporal_resolution = "1 year")
  cfg2 <- pg_config(end_date = as.Date("2024-09-01"), temporal_resolution = "1 year")
  expect_equal(priogrid:::get_temporal_hash(cfg1), priogrid:::get_temporal_hash(cfg2))
})
