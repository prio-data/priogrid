
#' gen_pop_gpw_sum 
#' 
#' @param path Path to gpw_nc NetCDF file. 
#' @export
gen_pop_gpw_sum <- function(path){
   gen_pop_gpw(path,sum)
}

#' gen_pop_gpw_sd 
#' 
#' @param path Path to gpw_nc NetCDF file. 
#' @export
gen_pop_gpw_sd <- function(path){
   gen_pop_gpw(path,sd)
}

#' gen_pop_gpw_min 
#' 
#' @param path Path to gpw_nc NetCDF file. 
#' @export
gen_pop_gpw_min <- function(path){
   gen_pop_gpw(path,min)
}

#' gen_pop_gpw_max 
#' 
#' @param path Path to gpw_nc NetCDF file. 
#' @export
gen_pop_gpw_max <- function(path){
   gen_pop_gpw(path,max)
}

## Function to generate pop_gpw_sum, _sd, _min and _max
## Returns RasterBrick for the selected variable for years 2000, 2005, 2010 and 2015.


#' Generate pop_gpw_ variables.
#'
#' @param gpw_nc Gridded Population of the World v.4 NetCDF file.
#' @param fun Function to aggregate by ('sum', 'sd', 'min', 'max')

gen_pop_gpw <- function(gpw_nc, fun){
  gpw <- raster::brick(gpw_nc)

  gpw <- gpw[[1:5]] # 1:5 to also included estimate for 2020
  years <- c(2000, 2005, 2010, 2015, 2020)

  gpw <- priogrid::prio_aggregate_raster(gpw, fun = fun)

  names(gpw) <- paste0("pop_gpw_", as.character(quote(rast.fun)), "_", years)
  return(gpw)
}
