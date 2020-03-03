
test_that("Blank priogrid is right size",{
   grid <- priogrid::prio_blank_grid()
   expect_equal(length(grid), 259200)
})
