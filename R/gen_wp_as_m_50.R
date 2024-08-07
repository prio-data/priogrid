#' wp_as_m_50_sum
#'
#' @description Generate yearly total number of people broken down by sex and age groupings sum variable, available from 2000-2020.
#' 
#' Please cite: WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00646
#'
#' source: https://hub.worldpop.org/doi/10.5258/SOTON/WP00654
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_wp_as_m_50_sum <- function(input_folder, fun = "sum"){
  
  years <- 2000:2020
  
  fnames <- paste0("global_m_50_", years, "_1km.tif")
  
  var <- paste0("wp_as_m_50_", as.character(fun))
  
  wp_as <- file.path(input_folder, "input", "worldpop", fnames) %>% 
    purrr::map(raster::raster) %>%
    purrr::map(priogrid::raster_to_pg, aggregation_function = fun) %>% 
    purrr::map(priogrid::raster_to_tibble, add_pg_index = TRUE) %>%
    purrr::map_dfr(tidyr::pivot_longer, cols = 3, names_to = "year", values_to = paste0(var)) %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(year, paste0(years, collapse = "|"))))
  
  return(wp_as)
}


#' wp_as_m_50_sd
#'
#' @description Generate yearly total number of people broken down by sex and age groupings sd variable, available from 2000-2020.
#' 
#' Please cite: WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00646
#'
#' source: https://hub.worldpop.org/doi/10.5258/SOTON/WP00654
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_wp_as_m_50_sd <- function(input_folder){
  gen_wp_as_m_50_sum(input_folder, fun = "sd")
}


#' wp_as_m_50_max
#'
#' @description Generate yearly total number of people broken down by sex and age groupings max variable, available from 2000-2020.
#' 
#' Please cite: WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00646
#'
#' source: https://hub.worldpop.org/doi/10.5258/SOTON/WP00654
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_wp_as_m_50_max <- function(input_folder){
  gen_wp_as_m_50_sum(input_folder, fun = "max")
}


#' wp_as_m_50_min
#'
#' @description Generate yearly total number of people broken down by sex and age groupings min variable, available from 2000-2020.
#' 
#' Please cite: WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00646
#'
#' source: https://hub.worldpop.org/doi/10.5258/SOTON/WP00654
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_wp_as_m_50_min <- function(input_folder){
  gen_wp_as_m_50_sum(input_folder, fun = "min")
}


#' wp_as_m_50_mean
#'
#' @description Generate yearly total number of people broken down by sex and age groupings mean variable, available from 2000-2020.
#' 
#' Please cite: WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00646
#'
#' source: https://hub.worldpop.org/doi/10.5258/SOTON/WP00654
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_wp_as_m_50_mean <- function(input_folder){
  gen_wp_as_m_50_sum(input_folder, fun = "mean")
}


#' wp_as_m_50_med
#'
#' @description Generate yearly total number of people broken down by sex and age groupings median variable, available from 2000-2020.
#' 
#' Please cite: WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00646
#'
#' source: https://hub.worldpop.org/doi/10.5258/SOTON/WP00654
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_wp_as_m_50_med <- function(input_folder){
  gen_wp_as_m_50_sum(input_folder, fun = "median")
}
