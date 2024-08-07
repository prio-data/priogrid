#' pop_ls_sum
#'
#' @description Generate yearly population sum variable, available from 2000-2022.
#' 
#' citations vary by year, look up: https://landscan.ornl.gov/citations
#' Please cite: Sims, K., Reith, A., Bright, E., Kaufman, J., Pyle, J., Epting, J., Gonzales, J., Adams, D., Powell, E., Urban, M., & Rose, A. (2023). LandScan Global 2022 [Data set]. Oak Ridge National Laboratory. https://doi.org/10.48690/1529167.
#'
#' source: https://landscan.ornl.gov/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_pop_ls_sum <- function(input_folder, fun = "sum"){
  
  years <- 2000:2022

  fnames <- paste0("landscan-global-", years, ".tif")
  
  var <- paste0("pop_ls_", as.character(fun))
  
  pop_ls <- file.path(input_folder, "input", "landscan_pop", fnames) %>% 
    purrr::map(raster::raster) %>%
    purrr::map(priogrid::raster_to_pg, aggregation_function = fun) %>% 
    purrr::map(priogrid::raster_to_tibble, add_pg_index = TRUE) %>%
    purrr::map_dfr(tidyr::pivot_longer, cols = 3, names_to = "year", values_to = paste0(var)) %>%
    dplyr::mutate(year = readr::parse_number(gsub('[[:punct:] ]+','', year)))  

  return(pop_ls)
}


#' pop_ls_sd
#'
#' @description Generate yearly population standard deviation variable, available from 2000-2022.
#' 
#' citations vary by year, look up: https://landscan.ornl.gov/citations
#' Please cite: Sims, K., Reith, A., Bright, E., Kaufman, J., Pyle, J., Epting, J., Gonzales, J., Adams, D., Powell, E., Urban, M., & Rose, A. (2023). LandScan Global 2022 [Data set]. Oak Ridge National Laboratory. https://doi.org/10.48690/1529167.
#'
#' source: https://landscan.ornl.gov/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_pop_ls_sd <- function(input_folder){
  gen_pop_ls_sum(input_folder, fun = "sd")
}


#' pop_ls_max
#'
#' @description Generate yearly population max variable, available from 2000-2022.
#' 
#' citations vary by year, look up: https://landscan.ornl.gov/citations
#' Please cite: Sims, K., Reith, A., Bright, E., Kaufman, J., Pyle, J., Epting, J., Gonzales, J., Adams, D., Powell, E., Urban, M., & Rose, A. (2023). LandScan Global 2022 [Data set]. Oak Ridge National Laboratory. https://doi.org/10.48690/1529167.
#'
#' source: https://landscan.ornl.gov/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_pop_ls_max <- function(input_folder){
  gen_pop_ls_sum(input_folder, fun = "max")
}


#' pop_ls_min
#'
#' @description Generate yearly population min variable, available from 2000-2022.
#' 
#' citations vary by year, look up: https://landscan.ornl.gov/citations
#' Please cite: Sims, K., Reith, A., Bright, E., Kaufman, J., Pyle, J., Epting, J., Gonzales, J., Adams, D., Powell, E., Urban, M., & Rose, A. (2023). LandScan Global 2022 [Data set]. Oak Ridge National Laboratory. https://doi.org/10.48690/1529167.
#'
#' source: https://landscan.ornl.gov/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_pop_ls_min <- function(input_folder){
  gen_pop_ls_sum(input_folder, fun = "min")
}


#' pop_ls_mean
#'
#' @description Generate yearly population mean variable, available from 2000-2022.
#' 
#' citations vary by year, look up: https://landscan.ornl.gov/citations
#' Please cite: Sims, K., Reith, A., Bright, E., Kaufman, J., Pyle, J., Epting, J., Gonzales, J., Adams, D., Powell, E., Urban, M., & Rose, A. (2023). LandScan Global 2022 [Data set]. Oak Ridge National Laboratory. https://doi.org/10.48690/1529167.
#'
#' source: https://landscan.ornl.gov/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_pop_ls_mean <- function(input_folder){
  gen_pop_ls_sum(input_folder, fun = "mean")
}


#' pop_ls_med
#'
#' @description Generate yearly population median variable, available from 2000-2022.
#' 
#' citations vary by year, look up: https://landscan.ornl.gov/citations
#' Please cite: Sims, K., Reith, A., Bright, E., Kaufman, J., Pyle, J., Epting, J., Gonzales, J., Adams, D., Powell, E., Urban, M., & Rose, A. (2023). LandScan Global 2022 [Data set]. Oak Ridge National Laboratory. https://doi.org/10.48690/1529167.
#'
#' source: https://landscan.ornl.gov/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_pop_ls_med <- function(input_folder){
  gen_pop_ls_sum(input_folder, fun = "median")
}
