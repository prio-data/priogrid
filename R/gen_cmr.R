
# Child malnutrition ------------------------------------------------------

#' Generate cmr variables RasterBrick.
#' 
#' @param cmr_data SEDAC Global Subnational Prevalence of Child Malnutrition grid data (v.1, uw.asc)


gen_cmr <- function(cmr_data){
  malnut <- raster::raster(cmr_data)
  
  # Value error in original data cf. PG2 .sql script
  malnut <- malnut/10
  
  cmr_mean <- priogrid::prio_aggregate_raster(malnut, fun = 'mean')
  cmr_sd <- priogrid::prio_aggregate_raster(malnut, rast.fun = 'sd')
  cmr_min <- priogrid::prio_aggregate_raster(malnut, rast.fun = 'min')
  cmr_max <- priogrid::prio_aggregate_raster(malnut, rast.fun = 'max')
  
  cmr <- raster::brick(c(cmr_mean, cmr_sd, cmr_min, cmr_max))
  names(cmr) <- c("cmr_mean", "cmr_sd", "cmr_min", "cmr_max")
  
  return(malnut)
}