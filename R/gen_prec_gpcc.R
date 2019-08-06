
#' Generate yearly precipitation variable (GPCC)
#'
#' Function to generate total yearly amount of precipitation
#' in each grid cell using monthly statistics from the Global Precipitation Climatology Centre.
#' Returns PRIO-GRID consistent RasterBrick of one layer for each year (1946-2016).
#'
#' @param gpcp_data GPCC Monthly total 1x1 NetCDF file.


gen_prec_gpcc <- function(gpcc_data){
  gpcc <- raster::brick(gpcc_data)
  
  gpcc <- gpcc[[which(raster::getZ(gpcc) >= as.Date("1946-01-01") 
                      & raster::getZ(gpcc) <= as.Date("2016-12-01"))]]
  
  
  index <- format(as.Date(names(gpcc), format = "X%Y.%m.%d"), format = "%Y")
  index <- as.numeric(index)
  
  fun <- function(x, na.rm = TRUE){tapply(x, index, sum)}
  
  gpcc <- raster::calc(gpcc, fun)
  
  gpcc <- priogrid::rotateExtent(gpcc)
  
  names(gpcc) <- paste0("prec_gpcc_", unique(index))
  
  gpcc <- raster::disaggregate(gpcc, fact = 2) # Unsure if method = 'bilinear' should be specified
  
  return(gpcc)
}



