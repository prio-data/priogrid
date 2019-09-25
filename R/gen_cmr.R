#' gen_cmr_mean 
#'
#' @param data uw.asc 
#' @export
gen_cmr_mean <- function(data){
   gen_cmr(data,mean)
}

#' gen_cmr_sd 
#'
#' @param data uw.asc 
#' @export
gen_cmr_sd <- function(data){
   gen_cmr(data,sd)
}

#' gen_cmr_min
#'
#' @param data uw.asc 
#' @export
gen_cmr_min <- function(data){
   gen_cmr(data,min)
}

#' gen_cmr_max 
#'
#' @param data uw.asc 
#' @export
gen_cmr_max <- function(data){
   gen_cmr(data,max)
}

# Child malnutrition ------------------------------------------------------

#' Generate cmr variables RasterBrick.
#' 
#' @param cmr_data SEDAC Global Subnational Prevalence of Child Malnutrition grid data (v.1, uw.asc)

gen_cmr <- function(data, fun){
  malnut <- raster::raster(data)
  
  # Value error in original data cf. PG2 .sql script
  malnut <- malnut/10
  
  priogrid::prio_aggregate_raster(malnut, fun = fun)
}
