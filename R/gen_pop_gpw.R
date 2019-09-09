## Function to generate pop_gpw_sum, _sd, _min and _max
## Returns RasterBrick for the selected variable for years 2000, 2005, 2010 and 2015.


#' Generate pop_gpw_ variables.
#'
#' @param gpw_nc Gridded Population of the World v.4 NetCDF file.
#' @param rast.fun Function to aggregate by ('sum', 'sd', 'min', 'max')


gen_pop_gpw <- function(gpw_nc, rast.fun){
  gpw <- raster::brick(gpw_nc)

  gpw <- gpw[[1:5]] # 1:5 to also included estimate for 2020
  years <- c(2000, 2005, 2010, 2015, 2020)

  gpw <- priogrid::prio_aggregate_raster(gpw, fun = rast.fun)

  names(gpw) <- paste0("pop_gpw_", rast.fun, "_", years)
  return(gpw)
}


# Function to combine all subvariables in one stack - can include this in above function

gen_pop_gpw_c <- function(gpw_nc){
  sum <- gen_pop_gpw(gpw_nc, rast.fun = 'sum')
  sd <- gen_pop_gpw(gpw_nc, rast.fun = 'sd')
  min <- gen_pop_gpw(gpw_nc, rast.fun = 'min')
  max <- gen_pop_gpw(gpw_nc, rast.fun = 'max')

  stack <- raster::stack(sum, sd, min, max)
  return(stack)

}


