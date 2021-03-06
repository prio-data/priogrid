
test_that("test auxillary control functions",{

   expect_error(specification_lookup("variable","gcp_mer"),NA)
   expect_error(specification_lookup("foo","myVar"),"Lookup type must be one of")
   expect_error(specification_lookup("variable","foo"),"specification in")

   expect_equal(get_in_path("in","foo"),"in/foo/data")
   expect_equal(get_out_path("out","foo",".rds"),"out/foo.rds")
   expect_equal(get_out_path("out","foo","csv"),"out/foo.csv")

})
