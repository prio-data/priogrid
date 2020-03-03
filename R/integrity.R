
#' @title check_folder_integrity
#' @description Checks that the contents of a folder correspond to a hash fingerprint.
#' @param folder The folder to check
#' @param variable The variable from which to get the hash value (to check) 
check_folder_integrity <- function(folder,variable){
   lookup <- variable_lookup(variable)
   actual <- get_folder_fingerprint(folder)
   lookup$hash == actual
}

#' @title get_folder_fingerprint
#' @description Gets a hash fingerprint, which can be used to ascertain that
#' the contents of a folder are intact.
#' @param folder
#' @export 

get_folder_fingerprint <- function(root){
   fp <- function(f){
      if(file.info(f)$isdir){
         get_folder_fingerprint(f)
      } else {
         tools::md5sum(f)
      }
   }
   files <- list.files(root,full.names = TRUE)
   hash <- do.call(paste0,lapply(files, fp))
   digest::digest(hash,algo = "md5")
}

