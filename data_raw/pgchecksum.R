# Do not run. (Used to test checksums for a verified set of files.)

destfolder <- pgoptions$get_rawfolder()
file_info <- pg_rawfiles()
pgchecksum <- file_info |> dplyr::mutate(
  md5 = tools::md5sum(file.path(destfolder, filename))
) |> dplyr::select(source_name, source_version, id, filename, md5)
usethis::use_data(pgchecksum, overwrite = FALSE)
