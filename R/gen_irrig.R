
#' Generate irrigation variables
#' 
#' Generate variables measuring area equipped for irrigation
#' in each grid cell based on the Historical Irrigation Dataset v. 1.0.
#' 
#' @param hid AEI_EARTHSTAT_IR ascii files (1950-2005)
#' @param fun Function describing variable to be generated ("sum", "sd", "min", "max")

gen_irrig <- function(hid1950, hid1960, hid1970, hid1980, hid1985, hid1990, hid1995, hid2000, hid2005, rast.fun){

  hid1950 <- raster::raster(hid1950)
  hid1960 <- raster::raster(hid1960) 
  hid1970 <- raster::raster(hid1970) 
  hid1980 <- raster::raster(hid1980) 
  hid1985 <- raster::raster(hid1985) 
  hid1990 <- raster::raster(hid1990) 
  hid1995 <- raster::raster(hid1995) 
  hid2000 <- raster::raster(hid2000) 
  hid2005 <- raster::raster(hid2005)
  
  hid <- raster::brick(hid1950, hid1960, hid1970, hid1980, hid1985, hid1990, hid1995, hid2000, hid2005)
  
  hid <- prio_aggregate_raster(hid, fun = rast.fun)
  
  raster::extent(hid) <- priogrid::prio_extent()
  

  years <- c(1950, 1960, 1970, 1980, 1985, 1990, 1995, 2000, 2005)
  names(hid) <- paste0("irrig_", rast.fun, "_" ,years)
  
  return(hid)
  
}