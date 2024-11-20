test_that("downloading file returns GET response", {
  url <- "https://httpbin.org/anything"
  tmp <- tempfile()
  res <- download_file_httr2(url, filepath = tmp)
  unlink(tmp)

  res |> testthat::expect_s3_class("httr2_response")
})
