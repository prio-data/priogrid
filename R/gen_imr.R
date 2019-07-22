
# Generate infant mortality rate variable ---------------------------------

#' Generate imr variables
#'
#' @param imr_data SEDAC Global Subnational Infant Mortality Rates grid data (v.2, GeoTIFF)
#' @param rast.fun Function to aggregate by ('mean', 'sd', 'min', 'max')


gen_imr <- function(imr_data, rast.fun){
  imr <- raster::raster(imr_data)

  # Set NA values < 0
  imr <- raster::reclassify(imr, rcl = cbind(-Inf, 0, NA), right = FALSE)

  # Aggregate to pg resolution
  imr <- raster::aggregate(imr, fact = priogrid::resolution_factor(imr),
                                fun = rast.fun, na.rm = TRUE)
  return(imr)

}



# Optional stack of sub-variables ~19 mins
gen_imr_stack <- function(imr_data){
  imr_mean <- gen_imr(imr_data, rast.fun = 'mean')
  imr_sd <- gen_imr(imr_data, rast.fun = 'sd')
  imr_min <- gen_imr(imr_data, rast.fun = 'min')
  imr_max <- gen_imr(imr_data, rast.fun = 'max')

  imr <- raster::stack(c(imr_mean, imr_sd, imr_min, imr_max))
  names(imr) <- c("imr_mean", "imr_sd", "imr_min", "imr_max")
  return(imr)

}



