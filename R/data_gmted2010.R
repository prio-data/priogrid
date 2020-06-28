#' gen_gmted2010
#'
#' Aggregates and transforms the gmted2010 topography data to PRIO-GRID. Source: https://www.usgs.gov/land-resources/eros/coastal-changes-and-impacts/gmted2010
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and gmted2010 elevation
#' @export
gen_gmted2010 <- function(input_folder){
  gmted2010 <- raster::raster(file.path(input_folder, "gmted2010", "mn30_grd", "w001001.adf"))
  gmted2010 <- raster::aggregate(gmted2010, fact = 60, fun = "mean") # aggregate data before reprojection to speed up.
  rast <- raster::projectRaster(from = gmted2010, crs = priogrid::prio_crs())
  rast <- priogrid::raster_to_pg(rast)
  names(rast) <- "gmted2010"
  gmted2010out <- priogrid::raster_to_tibble(rast,add_pg_index = TRUE)
  return(gmted2010out)
}
