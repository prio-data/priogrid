#' Reads the GHSL GHS Population Grid data
#'
#'
#' @return an object of class sf
#' @export
#' @references
#' \insertRef{schiavinaGHSPOPR2023AGHS2023}{priogrid}
read_ghsl_population_grid <- function(){
  zip_files <- get_pgfile(source_name = "GHSL GHS Population Grid",
                  source_version = "R2023",
                  id = "ae6a7612-4bef-452f-acd6-d2212cf9a7c5")

  zip_file <- zip_files[1]
  unzip(zip_file, list = T)
}
