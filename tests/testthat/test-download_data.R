# download_file_httr2 was made internal in the refactor.
# Download behaviour is covered indirectly by integration tests that call
# get_pgfile() / download_pg_rawdata() when raw data is present.
test_that("pg_data_availability returns NULL gracefully when rawfolder is unset", {
  skip_if(tryCatch({pg_rawfolder(); TRUE}, error = function(e) FALSE),
          "rawfolder already set in this environment")
  expect_null(pg_data_availability())
})

test_that("pg_data_availability returns a data frame when rawfolder is set", {
  skip_if_no_rawdata()
  res <- pg_data_availability()
  expect_s3_class(res, "data.frame")
  expect_true(all(c("source_name", "source_version", "n_files", "n_present",
                    "n_partial", "all_present") %in% names(res)))
  expect_gt(nrow(res), 0)
})

# ---- url list parsing ----

test_that("pg_default_filename strips the query string", {
  expect_equal(pg_default_filename("https://example.com/a/b/data.zip"), "data.zip")
  expect_equal(pg_default_filename("https://example.com/f.csv?versionId=2024"), "f.csv")
  expect_equal(pg_default_filename("https://ndownloader.figshare.com/files/17626052"), "17626052")
})

test_that("pg_read_url_list reads one- and two-column lines", {
  path <- withr::local_tempfile()
  writeLines(c("https://example.com/plain.zip",
               "https://example.com/files/123\tHarmonized_1992.tif",
               "",
               "https://example.com/other.csv"), path)

  res <- pg_read_url_list(path)

  expect_equal(nrow(res), 3)
  expect_equal(res$url, c("https://example.com/plain.zip",
                          "https://example.com/files/123",
                          "https://example.com/other.csv"))
  expect_equal(res$filename, c(NA, "Harmonized_1992.tif", NA))
})

test_that("pg_read_url_list errors clearly on a missing file", {
  expect_error(pg_read_url_list(""), "not found")
})

test_that("pg_format_url_list only writes filenames that differ from the url", {
  url <- c("https://example.com/plain.zip", "https://example.com/files/123")
  filename <- c("plain.zip", "Harmonized_1992.tif")

  expect_equal(pg_format_url_list(url, filename),
               c("https://example.com/plain.zip",
                 "https://example.com/files/123\tHarmonized_1992.tif"))
  expect_equal(pg_format_url_list(url, NULL), url)
})

test_that("pg_read_url_list and pg_format_url_list round-trip", {
  path <- withr::local_tempfile()
  lines <- c("https://example.com/plain.zip",
             "https://example.com/files/123\tHarmonized_1992.tif")
  writeLines(lines, path)

  res <- pg_read_url_list(path)
  expect_equal(pg_format_url_list(res$url, res$filename), lines)
})

test_that("pg_rawfiles gives every file a name and keeps them unique per source", {
  fi <- pg_rawfiles()

  expect_true(all(nzchar(basename(fi$filename))))
  expect_equal(anyDuplicated(paste(fi$id, fi$filename)), 0)
})

# ---- failure classification and backoff ----

test_that("pg_classify_download separates permanent from transient failures", {
  report <- data.frame(
    success     = c(TRUE, FALSE, FALSE, FALSE, NA,   FALSE, FALSE),
    status_code = c(200L, 404L,  403L,  503L,  NA,   429L,  0L)
  )

  expect_equal(pg_classify_download(report),
               c("ok", "permanent", "permanent", "transient",
                 "transient", "transient", "transient"))
})

test_that("pg_classify_download treats a successful transfer as ok whatever the status", {
  report <- data.frame(success = TRUE, status_code = 206L)
  expect_equal(pg_classify_download(report), "ok")
})

test_that("pg_retry_delay backs off exponentially and stays within the cap", {
  set.seed(1)
  delays <- vapply(1:8, function(a) pg_retry_delay(a, base = 2, cap = 120), numeric(1))

  expect_true(all(delays > 0))
  expect_true(all(delays <= 120))
  expect_true(all(diff(delays[1:6]) > 0))
  expect_equal(delays[8], 120)
})

test_that("pg_retry_delay adds jitter", {
  set.seed(42)
  delays <- vapply(1:50, function(i) pg_retry_delay(1, base = 2, cap = 120), numeric(1))
  expect_gt(length(unique(delays)), 1)
})

test_that("pg_retry_delay respects Retry-After but never exceeds the cap", {
  expect_gte(pg_retry_delay(1, base = 2, cap = 120, retry_after = 60), 60)
  expect_equal(pg_retry_delay(1, base = 2, cap = 120, retry_after = 9999), 120)
  expect_lt(pg_retry_delay(1, base = 2, cap = 120, retry_after = NA), 10)
})

# ---- header handling ----

test_that("pg_header_value takes the last occurrence and handles absence", {
  headers <- list("content-length" = "10", "content-length" = "20")
  expect_equal(pg_header_value(headers, "content-length"), "20")
  expect_true(is.na(pg_header_value(headers, "etag")))
  expect_true(is.na(pg_header_value(list(), "content-length")))
})

test_that("pg_disposition_filename handles both header forms", {
  expect_equal(pg_disposition_filename('attachment; filename="Harmonized_1992.tif"'),
               "Harmonized_1992.tif")
  expect_equal(pg_disposition_filename("attachment; filename=plain.zip"), "plain.zip")
  expect_equal(pg_disposition_filename("attachment; filename*=UTF-8''enc%20name.csv"),
               "enc name.csv")
  expect_true(is.na(pg_disposition_filename(NA_character_)))
  expect_true(is.na(pg_disposition_filename("inline")))
})

test_that("pg_expected_bytes reconstructs the full size of a resumed response", {
  # Plain response: Content-Length is the whole file.
  expect_equal(pg_expected_bytes(list("content-length" = "100"), 0, 200L), 100)

  # Resumed response: Content-Length is only what is left.
  expect_equal(pg_expected_bytes(list("content-length" = "40"), 60, 206L), 100)

  # Content-Range states the total outright and wins.
  expect_equal(pg_expected_bytes(list("content-length" = "40",
                                      "content-range" = "bytes 60-99/100"), 60, 206L), 100)

  # A server that ignored the range request sends the whole body with a 200.
  expect_equal(pg_expected_bytes(list("content-length" = "100"), 60, 200L), 100)
})

test_that("pg_expected_bytes gives up rather than guessing", {
  expect_true(is.na(pg_expected_bytes(list(), 0, 200L)))
  expect_true(is.na(pg_expected_bytes(list("content-length" = "100",
                                           "content-encoding" = "gzip"), 0, 200L)))
})

test_that("pg_suspicious_filename flags names that are not data files", {
  expect_false(pg_suspicious_filename("Harmonized_1992.tif"))
  expect_true(pg_suspicious_filename("17626052"))
  expect_true(pg_suspicious_filename(NA_character_))
  expect_true(pg_suspicious_filename("oauth.authorize"))
})

# ---- end to end, over file:// so no network is needed ----

local_fake_rawfolder <- function(env = parent.frame()) {
  folder <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(pg_rawfolder = function() folder, .env = env)
  folder
}

fake_file_info <- function(urls, filenames) {
  dplyr::tibble(
    source_name = "Test Source",
    source_version = "v1",
    id = "00000000-0000-0000-0000-000000000000",
    url = urls,
    filename = file.path("Test Source", "v1", "00000000-0000-0000-0000-000000000000", filenames)
  )
}

skip_if_no_file_protocol <- function() {
  probe <- withr::local_tempfile()
  writeLines("probe", probe)
  ok <- tryCatch({
    res <- curl::multi_download(paste0("file://", probe), withr::local_tempfile(),
                               progress = FALSE)
    isTRUE(res$success)
  }, error = function(e) FALSE)
  testthat::skip_if_not(ok, "curl cannot fetch file:// urls here")
}

test_that("download_pg_rawdata downloads, renames away .part, and reports per file", {
  skip_if_no_file_protocol()
  folder <- local_fake_rawfolder()

  source_file <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), source_file)

  fi <- fake_file_info(paste0("file://", source_file), "data.csv")
  res <- download_pg_rawdata(fi, quiet = TRUE)

  expect_equal(res$status, "ok")
  expect_equal(nrow(res), 1)
  expect_true(file.exists(file.path(folder, fi$filename)))
  expect_false(file.exists(paste0(file.path(folder, fi$filename), ".part")))
  expect_gt(res$bytes, 0)
})

test_that("download_pg_rawdata succeeds for a source with no reference checksum", {
  skip_if_no_file_protocol()
  local_fake_rawfolder()

  source_file <- withr::local_tempfile(fileext = ".csv")
  writeLines("bootstrap", source_file)

  fi <- fake_file_info(paste0("file://", source_file), "bootstrap.csv")

  # This is the first-download case: no pgchecksum row exists yet, so there is
  # nothing to compare against and that must not be treated as a problem.
  expect_no_warning(res <- download_pg_rawdata(fi, quiet = TRUE))
  expect_equal(res$status, "ok")
  expect_true(is.na(res$md5_ok))
})

test_that("download_pg_rawdata skips files that are already present", {
  skip_if_no_file_protocol()
  folder <- local_fake_rawfolder()

  source_file <- withr::local_tempfile(fileext = ".csv")
  writeLines("once", source_file)

  fi <- fake_file_info(paste0("file://", source_file), "once.csv")
  download_pg_rawdata(fi, quiet = TRUE)

  res <- download_pg_rawdata(fi, quiet = TRUE)
  expect_equal(res$status, "skipped")
})

test_that("download_pg_rawdata reports failure instead of returning quietly", {
  skip_if_no_file_protocol()
  folder <- local_fake_rawfolder()

  missing <- file.path(withr::local_tempdir(), "not-there.csv")
  fi <- fake_file_info(paste0("file://", missing), "not-there.csv")

  expect_warning(res <- download_pg_rawdata(fi, max_retry = 0, quiet = TRUE),
                 "could not be downloaded")
  expect_equal(res$status, "failed")
  expect_false(file.exists(file.path(folder, fi$filename)))
})

test_that("download_pg_rawdata rejects file_info without the expected columns", {
  local_fake_rawfolder()
  expect_error(download_pg_rawdata(dplyr::tibble(url = "https://example.com/a.zip")),
               "missing column")
})
