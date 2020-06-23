#' gen_ruggedness
#'
#' Aggregates and transforms the ruggedness data to PRIO-GRID.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and ruggedness
#' @export
gen_ruggedness <- function(input_folder){
  rugged <- raster::raster(file.path(input_folder, "ruggedness", "data", "ruggedness1K.tif"))
  rugged <- raster::aggregate(rugged, fact = 10, fun = "mean") # aggregate data before reprojection to speed up.
  rast <- raster::projectRaster(from = rugged, crs = priogrid::prio_crs())
  rast <- priogrid::raster_to_pg(rast)
  names(rast) <- "ruggedness"
  ruggedness <- priogrid::raster_to_tibble(rast)
  return(ruggedness)
}
