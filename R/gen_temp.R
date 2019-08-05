
#' Generate yearly mean temperature variable
#'
#' Function to generate average yearly temperature
#' in each grid cell using the GHCN_CAMS 2019 dataset.
#' Returns PRIO-GRID consistent RasterBrick of one layer for each year.
#'
#' @param temp_data GHCN_CAMS NetCDF dataset.

gen_temp <- function(temp_data){
  temp <- raster::brick(temp_data, varname = "air")

  index <- format(as.Date(raster::getZ(temp), format = "%Y-%m-%d %H:%M:%S"), format = "%Y")
  index <- as.numeric(index)

  fun <- function(x, na.rm = TRUE){tapply(x, index, mean)}
  temp <- raster::calc(temp, fun)

  temp <- temp - 273.15 # Convert from Kelvin to Celsius
  temp <- priogrid::rotateExtent(temp)

  names(temp) <- paste0("temp_", unique(index))

  temp <- raster::dropLayer(temp, 72)

}

