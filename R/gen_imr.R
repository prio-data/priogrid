
# Generate infant mortality rate variable ---------------------------------

#' Generate imr variables, returned as brick
#'
#' @param imr_data SEDAC Global Subnational Infant Mortality Rates grid data (v.2, GeoTIFF)


gen_imr <- function(imr_data){
  imr <- raster::raster(imr_data)

  imr <- raster::reclassify(imr, rcl = cbind(-Inf, 0, NA), right = FALSE)

  # Aggregate to pg resolution and create subvariables
  imr_mean <- priogrid::prio_aggregate_raster(imr, fun = 'mean')
  imr_sd <- priogrid::prio_aggregate_raster(imr, fun = 'sd')
  imr_min <- priogrid::prio_aggregate_raster(imr, fun = 'min')
  imr_max <- priogrid::prio_aggregate_raster(imr, fun = 'max')
  
  imr <- raster::brick(c(imr_mean, imr_sd, imr_min, imr_max))
  names(imr) <- c("imr_mean", "imr_sd", "imr_min", "imr_max")
  
  return(imr)

}

