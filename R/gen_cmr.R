
# Child malnutrition ------------------------------------------------------

#' Generate cmr variables
#' 
#' @param cmr_data SEDAC Globa Subnational Prevalence of Child Malnutrition grid data (v.1)
#' @param rast.fun Function to aggregate by ('mean', 'sd', 'min', 'max')


gen_cmr <- function(cmr_data, rast.fun){
  malnut <- raster::raster(cmr_data)
  
  # Value error in original data cf. PG2 sql script
  malnut <- malnut/10
  
  malnut <- raster::aggregate(malnut, fact = resolution_factor(malnut),
                      fun = rast.fun)
  return(malnut)
}


