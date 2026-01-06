test_that("resolve_pg_mode defaults to release mode", {

  cfg <- resolve_pg_mode()
  expect_equal(cfg$mode, "release")
  expect_equal(cfg$type, "05deg_yearly")
  expect_false(cfg$overwrite)
})

test_that("resolve_pg_mode with overwrite=TRUE defaults to custom mode", {
  cfg <- resolve_pg_mode(overwrite = TRUE)
  expect_equal(cfg$mode, "custom")
  expect_true(cfg$overwrite)
})

test_that("resolve_pg_mode warns and ignores overwrite for releases", {
  cfg <- resolve_pg_mode(version = "3.0.0", overwrite = TRUE)
  expect_equal(cfg$mode, "release")
  expect_false(cfg$overwrite)
  expect_false(is.null(cfg$warning))
})

test_that("resolve_pg_mode errors on mixed params", {
  expect_error(
    resolve_pg_mode(version = "3.0.0", spatial_hash = "abc123"),
    "Cannot mix"
  )
})

test_that("resolve_pg_mode errors on partial hash params", {
  expect_error(
    resolve_pg_mode(spatial_hash = "abc123"),
    "Both spatial_hash and temporal_hash"
  )
})
