test_that(".pg_release_manifest resolves the 3.0.2 release manifest", {
  m <- priogrid:::.pg_release_manifest("3.0.2", "05deg_yearly")
  expect_equal(nrow(m), 40L)
  expect_true(all(startsWith(m$url, "https://cdn.cloud.prio.org/files/")))
  expect_true(all(c("bdist1.tif", "cshapes_gwcode.tif", "ne_disputed_area_share.tif",
                    "_checksums.csv", "pg_config.json") %in% m$filename))
  expect_true(all(paste0(pgvariables$name, ".tif") %in% m$filename))
})

test_that(".pg_release_manifest errors for a release with no manifest", {
  expect_error(priogrid:::.pg_release_manifest("3.0.1", "05deg_yearly"), "manifest")
})

test_that("download_priogrid(list_releases = TRUE) lists only manifest-backed releases", {
  df <- download_priogrid(list_releases = TRUE)
  expect_true("3.0.2" %in% df$version)
  expect_false("3.0.1" %in% df$version)
})
