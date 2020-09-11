#' nightlights
#'
#' @description Generate yearly nighttime light variable, available from 1992-2018.
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_nightlights <- function(input_folder){
  years <- 1992:2013
  years_sim <- 2014:2018

  fname <- paste0("Harmonized_DN_NTL_", years, "_calDMSP", ".tif")
  fname_sim <- paste0("Harmonized_DN_NTL_", years_sim, "_simVIIRS", ".tif")
  fnames <- c(fname, fname_sim)

  nlights <- file.path(input_folder, "nightlights", "data", fnames) %>% purrr::map(raster::raster) %>%
    purrr::map(raster_to_pg) %>% purrr::map(priogrid::raster_to_tibble, add_pg_index = TRUE) %>%
    purrr::map_dfr(tidyr::pivot_longer, cols = 3, names_to = "year", values_to = "nightlights") %>%
    dplyr::mutate(year = readr::parse_number(year))

  return(nlights)
}
