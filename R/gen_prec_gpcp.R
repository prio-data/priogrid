
#' Generate yearly precipitation variable (GPCP)
#'
#' Function to generate total yearly amount of precipitation
#' in each grid cell using the GPCP Version 2.3 dataset.
#' Returns PRIO-GRID consistent RasterBrick of one layer for each year.
#'
#' @param gpcp_data GPCP Monthly Mean NetCDF file.


gen_prec_gpcp <- function(gpcp_data){

  prec <- raster::brick(gpcp_data)

  prec <- prec * lubridate::days_in_month(raster::getZ(prec)) # Get monthly precipitation

  index <- format(as.Date(names(prec), format = "X%Y.%m.%d"), format = "%Y")
  index <- as.numeric(index)

  yearly <- raster::stackApply(prec, index, fun = sum)

  prec.r <- priogrid::rotateExtent(yearly)
  names(prec.r) <- paste0("prec_gpcp_", unique(index))
  
  prec.r <- raster::dropLayer(prec.r, 41) # Drop 2019 - precipitation only recorded from January to May

  disagg <- raster::disaggregate(prec.r, fact = 5, method = 'bilinear') # Unsure if method is correct
  disagg
}
