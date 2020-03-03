
test_that("Test file integrity functions",{
   tempfolder <- tempdir()
   writeLines("foo",file.path(tempfolder,"alpha"))
   writeLines("bar",file.path(tempfolder,"beta"))
   writeLines("baz",file.path(tempfolder,"gamma"))
   saveRDS(mtcars,file.path(tempfolder,"bytes.rds"))

   fp <- get_folder_fingerprint(tempfolder)
   # is deterministic
   expect_equal(fp,get_folder_fingerprint(tempfolder))

   # Fails when something is added 
   saveRDS(mtcars,file.path(tempfolder,"bytes2.rds"))
   expect_false(fp==get_folder_fingerprint(tempfolder))

   # Succeeds again 
   file.remove(file.path(tempfolder,"bytes2.rds"))
   expect_equal(fp,get_folder_fingerprint(tempfolder))

   # Fails when something is removed 
   file.remove(file.path(tempfolder,"bytes.rds"))
   expect_false(fp==get_folder_fingerprint(tempfolder))
   saveRDS(mtcars,file.path(tempfolder,"bytes.rds"))
   expect_equal(fp,get_folder_fingerprint(tempfolder))

   dir.create(file.path(tempfolder,"adir"))
   writeLines("eep",file.path(tempfolder,"adir","afile"))
   expect_false(fp==get_folder_fingerprint(tempfolder))
})
