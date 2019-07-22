
# Child malnutrition ------------------------------------------------------

#' Generate cmr variables
#' 
#' @param cmr_data SEDAC Global Subnational Prevalence of Child Malnutrition grid data (v.1)
#' @param rast.fun Function to aggregate by ('mean', 'sd', 'min', 'max')


gen_cmr <- function(cmr_data, rast.fun){
  malnut <- raster::raster(cmr_data)
  
  # Value error in original data cf. PG2 .sql script
  malnut <- malnut/10
  
  malnut <- raster::aggregate(malnut, fact = priogrid::resolution_factor(malnut),
                      fun = rast.fun)
  return(malnut)
}


## Used file 'Uwq/uwq.asc' from the hunger-grid zip folder


# Stack of sub-variables
gen_cmr_stack <- function(cmr_data){
  cmr_mean <- gen_cmr(cmr_data, rast.fun = 'mean')
  cmr_sd <- gen_cmr(cmr_data, rast.fun = 'sd')
  cmr_min <- gen_cmr(cmr_data, rast.fun = 'min')
  cmr_max <- gen_cmr(cmr_data, rast.fun = 'max')
  
  cmr <- raster::stack(c(cmr_mean, cmr_sd, cmr_min, cmr_max))
  names(cmr) <- c("cmr_mean", "cmr_sd", "cmr_min", "cmr_max")
  return(cmr)
  
}


