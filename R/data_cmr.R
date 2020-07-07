#' @title cmr_mean
#'
#' @description Measures the average prevalence of child malnutrition within each cell. The original pixel value
#' is the percent of children under the age of 5 that are malnourished. Based on raster data from the SEDAC Global
#' Poverty Mapping project. The indicator is a snapshot for the year 2000 only.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2005. Poverty Mapping Project: Global Subnational Prevalence of Child Malnutrition. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4K64G12.
#'
#' @param input_folder Path to [pg-folder].
#' @param fun one of c("mean", "sd", "min", "max")
#'
#' @export
gen_cmr_mean <- function(input_folder, fun = "mean"){
   malnut <- raster::raster(file.path(input_folder, "cmr", "data", "uw.asc"))
   malnut <- malnut/10

   malnut <- priogrid::raster_to_pg(malnut, aggregation_function = fun)

   malnut <- priogrid::raster_to_tibble(malnut, add_pg_index = TRUE)

   names(malnut)[3] <- paste0("cmr_", as.character(fun))

   return(malnut)
}


#' @title cmr_sd
#'
#' @description Measures the prevalence of child malnutrition within each cell. The original pixel value
#' is the percent of children under the age of 5 that are malnourished. This variable gives the standard deviation
#' of original pixel values within each cell. Based on raster data from the SEDAC Global Poverty Mapping
#' project. The indicator is a snapshot for the year 2000 only.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2005. Poverty Mapping Project: Global Subnational Prevalence of Child Malnutrition. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4K64G12.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_cmr_sd <- function(input_folder){
   priogrid::gen_cmr(input_folder, fun = "sd")
}

#' @title cmr_min
#'
#' @description Measures the prevalence of child malnutrition within each cell. The original pixel value
#' is the percent of children under the age of 5 that are malnourished. This variable gives the minimum
#' of original pixel values within each cell. Based on raster data from the SEDAC Global Poverty Mapping
#' project. The indicator is a snapshot for the year 2000 only.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2005. Poverty Mapping Project: Global Subnational Prevalence of Child Malnutrition. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4K64G12.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_cmr_min <- function(input_folder){
   priogrid::gen_cmr(input_folder, fun = "min")
}


#' @title cmr_max
#'
#' @description Measures the prevalence of child malnutrition within each cell. The original pixel value
#' is the percent of children under the age of 5 that are malnourished. This variable gives the maximum
#' of original pixel values within each cell. Based on raster data from the SEDAC Global Poverty Mapping
#' project. The indicator is a snapshot for the year 2000 only.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2005. Poverty Mapping Project: Global Subnational Prevalence of Child Malnutrition. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4K64G12.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_cmr_max <- function(input_folder){
   priogrid::gen_cmr(input_folder, fun = "max")
}
