test_that("resolve_pg_mode defaults to release mode", {
  cfg <- resolve_pg_mode()
  expect_equal(cfg$mode, "release")
  expect_equal(cfg$type, "05deg_yearly")
  expect_false(cfg$overwrite)
})

test_that("resolve_pg_mode with config is custom mode", {
  cfg <- resolve_pg_mode(config = pg_config())
  expect_equal(cfg$mode, "custom")
  expect_false(cfg$overwrite)
})

test_that("resolve_pg_mode with config and overwrite=TRUE is custom mode", {
  cfg <- resolve_pg_mode(config = pg_config(), overwrite = TRUE)
  expect_equal(cfg$mode, "custom")
  expect_true(cfg$overwrite)
})

test_that("resolve_pg_mode warns and ignores overwrite for releases", {
  cfg <- resolve_pg_mode(version = "3.0.0", overwrite = TRUE)
  expect_equal(cfg$mode, "release")
  expect_false(cfg$overwrite)
  expect_false(is.null(cfg$warning))
})

test_that("resolve_pg_mode release mode stores release config", {
  cfg <- resolve_pg_mode(version = "3.0.1")
  expect_equal(cfg$mode, "release")
  expect_s3_class(cfg$config, "pg_config")
  expect_equal(cfg$config$nrow, 360L)
})

test_that("resolve_pg_mode errors on mixed config and version", {
  expect_error(
    resolve_pg_mode(config = pg_config(), version = "3.0.0"),
    "Cannot mix config with version"
  )
})

test_that("resolve_pg_mode errors on mixed hashes and version", {
  expect_error(
    resolve_pg_mode(version = "3.0.0", spatial_hash = "abc123", temporal_hash = "xyz789"),
    "Cannot mix release parameters"
  )
})

test_that("resolve_pg_mode errors on partial hash params", {
  expect_error(
    resolve_pg_mode(spatial_hash = "abc123"),
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

test_that("calc_pg writes _config.R on first creation", {
  skip_if_not_installed("terra")
  cfg <- test_config()

  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  withr::with_envvar(list(), {
    pg_set_rawfolder(tmp_raw)
    calc_pg(pgvariables$name[1], config = cfg)

    s_hash <- priogrid:::get_spatial_hash(cfg)
    t_hash <- priogrid:::get_temporal_hash(cfg)
    config_file <- file.path(
      pg_rawfolder(), "priogrid", "custom",
      as.character(packageVersion("priogrid")),
      s_hash, t_hash, "_config.R"
    )
    expect_true(file.exists(config_file))

    content <- readLines(config_file)
    expect_true(any(grepl("pg_config", content)))
    expect_true(any(grepl(as.character(cfg$nrow), content)))
  })
})

test_that("pg_list_custom returns configs and prints summary", {
  skip_if_not_installed("terra")
  cfg <- test_config()

  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  pg_set_rawfolder(tmp_raw)
  calc_pg(pgvariables$name[1], config = cfg)

  output <- capture.output(customs <- pg_list_custom())
  expect_length(customs, 1L)
  expect_s3_class(customs[[1]], "pg_config")
  expect_true(any(grepl("\\[1\\]", output)))
})
