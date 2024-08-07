#' nl_harm_sum
#'
#' @description Generate yearly nighttime light variable, available from 1992-2021.
#' 
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#' 
#' source: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/8
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_nl_harm_sum <- function(input_folder, fun = "sum"){
  
  years <- 1992:2013
  years_sim <- 2014:2021

  fname <- paste0("Harmonized_DN_NTL_", years, "_calDMSP", ".tif")
  fname_sim <- paste0("Harmonized_DN_NTL_", years_sim, "_simVIIRS", ".tif")
  fnames <- c(fname, fname_sim)
  
  var <- paste0("nl_harm_", as.character(fun))
  
  nlights <- file.path(input_folder, "input", "nl_harmonized_dmsp_viirs", fnames) %>% 
    purrr::map(raster::raster) %>%
    purrr::map(raster_to_pg, aggregation_function = fun) %>% 
    purrr::map(priogrid::raster_to_tibble, add_pg_index = TRUE) %>%
    purrr::map_dfr(tidyr::pivot_longer, cols = 3, names_to = "year", values_to = paste0(var)) %>%
    dplyr::mutate(year = readr::parse_number(year))
  
  return(nlights)
}

#' nl_harm_sd
#'
#' @description Generate yearly nighttime light variable, available from 1992-2021.
#' 
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#' 
#' source: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/8
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_harm_sd <- function(input_folder){
  gen_nl_harm_sum(input_folder, fun = "sd")
}


#' nl_harm_max
#'
#' @description Generate yearly nighttime light variable, available from 1992-2021.
#' 
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#' 
#' source: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/8
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_harm_max <- function(input_folder){
  gen_nl_harm_sum(input_folder, fun = "max")
}


#' nl_harm_min
#'
#' @description Generate yearly nighttime light variable, available from 1992-2021.
#' 
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#' 
#' source: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/8
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_harm_min <- function(input_folder){
  gen_nl_harm_sum(input_folder, fun = "min")
}


#' nl_harm_avg
#'
#' @description Generate yearly nighttime light variable, available from 1992-2021.
#' 
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#' 
#' source: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/8
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_harm_avg <- function(input_folder){
  gen_nl_harm_sum(input_folder, fun = "mean")
}


#' nl_harm_med
#'
#' @description Generate yearly nighttime light variable, available from 1992-2021.
#' 
#' Please cite: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020). https://doi.org/10.1038/s41597-020-0510-y
#' 
#' source: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/8
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_harm_med <- function(input_folder){
  gen_nl_harm_sum(input_folder, fun = "median")
}
