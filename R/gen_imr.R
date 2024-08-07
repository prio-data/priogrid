#' @title imr2000
#'
#' @description Generate cell infant mortality rate
#' for the year 2000 from
#' the Global Subnational Infant Mortality Rates, v1 (2000) dataset.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-infant-mortality-rates.
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2005. Poverty Mapping Project: Global Subnational Infant Mortality Rates. Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4PZ56R2. Accessed DAY MONTH YEAR.
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_imr_2000 <- function(input_folder, fun){
  
  file <- file.path(input_folder, "input", "imr", "2000", "imr.shp")
  
  imr_shp <- sf::st_read(file)
  
  imr_shp <- sf::st_transform(imr_shp, crs = priogrid::prio_crs())
  
  imr <- priogrid::vector_to_pg(imr_shp, variable = "IMR", fun = "first", need_aggregation = TRUE)
  
  imr <- priogrid::raster_to_tibble(imr, add_pg_index = TRUE)
  
  imr <- imr %>%
    dplyr::mutate(IMR_2000 = ifelse(IMR == -9999.0, NA, IMR)) %>%
    dplyr::select(-IMR)
  
  return(imr)
  
}


#' @title imr2015
#'
#' @description Generate cell infant mortality rate
#' for the year 2015 from
#' the Global Subnational Infant Mortality Rates, v2.01 (2015) dataset.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-infant-mortality-rates-v2-01.
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2021. Global Subnational Infant Mortality Rates, Version 2.01. Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/0gdn-6y33. Accessed DAY MONTH YEAR.
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_imr_2015 <- function(input_folder, fun){
  
  file <- file.path(input_folder, "input", "imr", "2015", "povmap_global_subnational_infant_mortality_rates_v2_01.tif")
  
  imr_tif <- raster::raster(file)
  
  imr <- priogrid::raster_to_pg(imr_tif, aggregation_function = fun)
  
  imr <- priogrid::raster_to_tibble(imr, add_pg_index = TRUE)
  
  imr <- imr %>%
    dplyr::mutate(IMR_2015 = ifelse(povmap_global_subnational_infant_mortality_rates_v2_01 < 0, NA, povmap_global_subnational_infant_mortality_rates_v2_01)) %>%
    dplyr::select(-povmap_global_subnational_infant_mortality_rates_v2_01)
  
  return(imr)
  
}
