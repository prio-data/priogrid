
test_that("test auxillary control functions",{

   expect_error(lookup("variable","myVar"),NA)
   expect_error(lookup("foo","myVar"),"Lookup type must be one of")
   expect_error(lookup("variable","foo"),"specification in")

   expect_equal(getInPath("in","foo"),"in/foo/data")
   expect_equal(getOutPath("out","foo",".rds"),"out/foo.rds")
   expect_equal(getOutPath("out","foo","csv"),"out/foo.csv")

})
