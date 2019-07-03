
test_that("Blank priogrid is right size",{
   grid <- priogrid::blank_pgrid()
   expect_equal(length(grid), 259200)
})
