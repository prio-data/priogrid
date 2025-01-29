testthat::test_that("url parsing works", {
  testthat::expect_equal(parse_url("   "), "missing")
  testthat::expect_equal(parse_url("urls/missing"), "missing url file")
  testthat::expect_equal(parse_url("urls/9e85ae0c-c773-4636-a614-3933903e848c.txt"), "parsed url file")
  tmp <- tempfile()
  writeLines("test", tmp)
  testthat::expect_equal(parse_url(tmp), "unparsed url file")
  unlink(tmp)
  if(curl::has_internet()){
    testthat::expect_equal(parse_url("www.vg.no"), "working url")
    testthat::expect_equal(parse_url("hmm"), "possibly non-working url")
  }
})
