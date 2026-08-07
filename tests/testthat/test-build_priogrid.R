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
    calc_pg("naturalearth_cover_share", config = cfg)

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
  calc_pg("naturalearth_cover_share", config = cfg)

  output <- capture.output(customs <- pg_list_custom())
  expect_length(customs, 1L)
  expect_s3_class(customs[[1]], "pg_config")
  expect_true(any(grepl("\\[1\\]", output)))
})

test_that(".pg_bootstrap_checksums adds entries for existing files", {
  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  pg_set_rawfolder(tmp_raw)
  cfg <- pg_config()
  s_hash <- priogrid:::get_spatial_hash(cfg)
  t_hash <- priogrid:::get_temporal_hash(cfg)
  out_path <- pgout_path(spatial_hash = s_hash, temporal_hash = t_hash)
  dir.create(out_path, recursive = TRUE)

  dir.create(file.path(out_path, "cog"))
  terra::writeRaster(terra::rast(nrows = 2L, ncols = 2L, vals = 1:4),
                     file.path(out_path, "cog", "fake_var.tif"),
                     filetype = "COG", overwrite = TRUE)

  result <- priogrid:::.pg_bootstrap_checksums(config = cfg)

  checksum_file <- file.path(out_path, "_checksums.csv")
  expect_true(file.exists(checksum_file))
  cs <- utils::read.csv(checksum_file, stringsAsFactors = FALSE)
  expect_true("fake_var" %in% cs$varname)
  expect_equal(result$n_added, 1L)
  expect_equal(result$n_updated, 0L)
})

test_that(".pg_bootstrap_checksums skips existing entries by default", {
  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  pg_set_rawfolder(tmp_raw)
  cfg <- pg_config()
  s_hash <- priogrid:::get_spatial_hash(cfg)
  t_hash <- priogrid:::get_temporal_hash(cfg)
  out_path <- pgout_path(spatial_hash = s_hash, temporal_hash = t_hash)
  dir.create(out_path, recursive = TRUE)

  dir.create(file.path(out_path, "cog"))
  terra::writeRaster(terra::rast(nrows = 2L, ncols = 2L, vals = 1:4),
                     file.path(out_path, "cog", "fake_var.tif"),
                     filetype = "COG", overwrite = TRUE)
  priogrid:::.pg_bootstrap_checksums(config = cfg)

  result2 <- priogrid:::.pg_bootstrap_checksums(config = cfg)
  expect_equal(result2$n_added, 0L)
  expect_equal(result2$n_updated, 0L)
})


# ---- Hive-partitioned builder tests ----

test_that(".pg_build_timevarying writes hive partitions, CSV bundle, and manifest", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  pg_set_rawfolder(tmp_raw)
  cfg  <- test_config()
  base <- pgout_path(config = cfg)
  dir.create(base, recursive = TRUE)

  # Fabricate two time-varying COG rasters
  tv <- pgvariables$name[!pgvariables$static][1:2]
  dates <- pg_dates(cfg)

  for (i in seq_along(tv)) {
    r <- prio_blank_grid(cfg)
    for (j in seq_along(dates)[-1]) terra::add(r) <- prio_blank_grid(cfg)
    terra::values(r) <- runif(terra::ncell(r) * terra::nlyr(r))
    names(r) <- as.character(dates)
    # First variable: set 2011-12-31 layer all-NA
    if (i == 1L) terra::values(r[[which(dates == as.Date("2011-12-31"))]]) <- NA_real_
    save_pgvariable(r, tv[i], save_to = base)
  }

  build_pg_dataset(config = cfg)

  # Hive partitions exist
  for (yr in c("year=2010", "year=2011", "year=2012")) {
    expect_true(file.exists(file.path(base, "timevarying", yr, "part-0.parquet")))
  }
  # CSV bundle and manifest exist
  expect_true(file.exists(file.path(base, "pg_timevarying.csv.gz")))
  expect_true(file.exists(file.path(base, "pg_config.json")))
  # Monolith must be absent
  expect_false(file.exists(file.path(base, "pg_timevarying.parquet")))

  # Read hive dataset and check schema / content
  d <- data.table::setDT(dplyr::collect(arrow::open_dataset(file.path(base, "timevarying"))))
  expect_true("year" %in% names(d))
  expect_true(all(tv %in% names(d)))
  expect_true("pgid" %in% names(d))
  expect_true("measurement_date" %in% names(d))
  expect_equal(sort(unique(d$year)), 2010:2012)

  # First variable all-NA in 2011; second populated
  d2011 <- d[d$year == 2011, ]
  expect_true(all(is.na(d2011[[tv[1]]])))
  expect_true(any(!is.na(d2011[[tv[2]]])))

  # Manifest assertions
  m <- jsonlite::read_json(file.path(base, "pg_config.json"), simplifyVector = TRUE)
  expect_equal(m$grid$nrow, 5L)
  expect_equal(m$grid$ncol, 10L)
  expect_equal(m$temporal$partition_key, "year")
  expect_equal(m$grid$extent$xmin, -180)
  expect_true(setequal(m$timevarying_variables, tv))
  expect_true(setequal(m$partitions, c("year=2010", "year=2011", "year=2012")))

  # COG files written by save_pgvariable
  for (vn in tv) {
    expect_true(file.exists(file.path(base, "cog", paste0(vn, ".tif"))))
  }
  # Manifest carries variables metadata
  expect_true("variables" %in% names(m))
  expect_true(all(tv %in% names(m$variables)))

  # CSV bundle row count matches hive
  csv <- data.table::fread(file.path(base, "pg_timevarying.csv.gz"))
  expect_equal(nrow(csv), nrow(d))
  expect_true(setequal(setdiff(names(d), "year"), names(csv)))
})

test_that("read_pg_timevarying reads cached hive dataset (no terra required)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  pg_set_rawfolder(tmp_raw)
  cfg  <- test_config()
  base <- pgout_path(config = cfg)
  dir.create(base, recursive = TRUE)

  tv <- pgvariables$name[!pgvariables$static][1:2]
  dates <- pg_dates(cfg)
  for (i in seq_along(tv)) {
    r <- prio_blank_grid(cfg)
    for (j in seq_along(dates)[-1]) terra::add(r) <- prio_blank_grid(cfg)
    terra::values(r) <- runif(terra::ncell(r) * terra::nlyr(r))
    names(r) <- as.character(dates)
    save_pgvariable(r, tv[i], save_to = base)
  }

  build_pg_dataset(config = cfg)
  d_ref <- data.table::setDT(dplyr::collect(arrow::open_dataset(file.path(base, "timevarying"))))

  x <- read_pg_timevarying(config = cfg)
  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), nrow(d_ref))
})

test_that("read_pg_timevarying falls back to legacy pg_timevarying.parquet", {
  skip_if_not_installed("arrow")

  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  on.exit(unlink(tmp_raw, recursive = TRUE), add = TRUE)

  pg_set_rawfolder(tmp_raw)
  cfg2 <- pg_config(nrow = 5L, ncol = 10L,
                    start_date = as.Date("2000-12-31"),
                    end_date   = as.Date("2000-12-31"),
                    temporal_resolution = "1 year")
  base2 <- pgout_path(config = cfg2)
  dir.create(base2, recursive = TRUE)

  legacy_df <- data.frame(pgid = 1:3,
                          measurement_date = as.Date("2000-12-31"),
                          x = 1:3)
  arrow::write_parquet(legacy_df, file.path(base2, "pg_timevarying.parquet"))

  result <- read_pg_timevarying(config = cfg2)
  expect_equal(nrow(result), 3L)
  expect_equal(sort(result$pgid), 1:3)
})

# ---------------------------------------------------------------------------
# read_pg_timevarying subsetting tests
# ---------------------------------------------------------------------------

.make_timevarying_fixture <- function() {
  tmp_raw <- tempfile()
  dir.create(tmp_raw)
  pg_set_rawfolder(tmp_raw)
  cfg  <- test_config()
  base <- pgout_path(config = cfg)
  dir.create(base, recursive = TRUE)

  tv <- pgvariables$name[!pgvariables$static][1:2]
  dates <- pg_dates(cfg)
  for (i in seq_along(tv)) {
    r <- prio_blank_grid(cfg)
    for (j in seq_along(dates)[-1]) terra::add(r) <- prio_blank_grid(cfg)
    terra::values(r) <- runif(terra::ncell(r) * terra::nlyr(r))
    names(r) <- as.character(dates)
    save_pgvariable(r, tv[i], save_to = base)
  }
  build_pg_dataset(config = cfg)
  list(cfg = cfg, base = base, tv = tv, tmp_raw = tmp_raw)
}

test_that("read_pg_timevarying years filter returns only requested years", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  x <- read_pg_timevarying(config = fx$cfg, years = 2011)
  expect_s3_class(x, "data.table")
  expect_equal(unique(x$year), 2011L)
  full <- read_pg_timevarying(config = fx$cfg)
  expect_true(nrow(x) < nrow(full))
})

test_that("read_pg_timevarying date range filter keeps only matching rows", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  x <- read_pg_timevarying(config = fx$cfg,
                            start_date = as.Date("2011-01-01"),
                            end_date   = as.Date("2011-12-31"))
  expect_true(all(lubridate::year(x$measurement_date) == 2011L))
})

test_that("read_pg_timevarying pgids filter keeps only requested cells", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  x <- read_pg_timevarying(config = fx$cfg, pgids = c(1L, 2L, 3L))
  expect_true(all(x$pgid %in% 1:3))
})

test_that("read_pg_timevarying extent filter returns non-empty subset of valid pgids", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  ext <- c(-180, -90, -90, 0)
  x <- read_pg_timevarying(config = fx$cfg, extent = ext)
  expected_pgids <- priogrid:::.pg_extent_to_pgids(ext, fx$cfg)
  expect_true(nrow(x) > 0)
  expect_true(all(x$pgid %in% expected_pgids))
})

test_that("read_pg_timevarying variables filter projects to requested columns", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  x <- read_pg_timevarying(config = fx$cfg, variables = fx$tv[1])
  expect_setequal(names(x), c("pgid", "measurement_date", "year", fx$tv[1]))
})

test_that("read_pg_timevarying combined filters all hold simultaneously", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  x <- read_pg_timevarying(config = fx$cfg,
                            years = 2011, pgids = 1:3, variables = fx$tv[1])
  expect_true(all(x$pgid %in% 1:3))
  expect_equal(unique(x$year), 2011L)
  expect_setequal(names(x), c("pgid", "measurement_date", "year", fx$tv[1]))
})

test_that("read_pg_timevarying errors when subsetting with as_raster=TRUE", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  expect_error(
    read_pg_timevarying(config = fx$cfg, years = 2011, as_raster = TRUE),
    "only"
  )
})

test_that("read_pg_timevarying errors on unknown variable name", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("terra")

  fx <- .make_timevarying_fixture()
  on.exit(unlink(fx$tmp_raw, recursive = TRUE), add = TRUE)

  expect_error(
    read_pg_timevarying(config = fx$cfg, variables = "nope"),
    "Unknown variable"
  )
})

test_that(".pg_extent_to_pgids reprojects lon/lat extent for non-4326 configs", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # UTM zone 32N over a small regional extent. Resolution is defined in 4326;
  # terra determines the projected raster's actual dimensions, which may differ
  # from nrow/ncol — that is expected behaviour.
  cfg_proj <- pg_config(
    nrow   = 10L, ncol = 20L,
    crs    = "epsg:32632",
    extent = c(xmin = 0, xmax = 10, ymin = 44, ymax = 48)
  )

  # Find which pgids exist in the projected grid.
  pg          <- prio_blank_grid(cfg_proj)
  valid_pgids <- as.integer(sort(unique(terra::values(pg)[!is.na(terra::values(pg))])))
  skip_if(length(valid_pgids) == 0L, "projection yielded no cells")

  # A lon/lat box covering the full config extent.
  # Before the fix: raw degree values (0..10, 44..48) were passed to terra::crop
  # as UTM metre coordinates — a ~10 m × ~10 m box near the origin — returning
  # integer(0). After the fix the lon/lat box is reprojected to UTM metres first.
  result <- priogrid:::.pg_extent_to_pgids(c(0, 10, 44, 48), cfg_proj)
  expect_type(result, "integer")
  expect_true(length(result) > 0L)
  expect_true(all(result %in% valid_pgids))
})