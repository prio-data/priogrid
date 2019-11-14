
#' Generate Life Expectancy variable from SHDI
#'
#' Average Life expectancy at birth within each grid cell.
#'
#' @param shdi_data SHDI .csv data.
#' @param shdi_sf SHDI MultiPolygon shapefile.



gen_lifexp <- function(path){
  shdi <- read.csv(file.path(path,"SHDI-Complete SD1.csv"))
  geom <- sf::read_sf(file.path(path,"GDL-SHDI-SHP-2.shp"))

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, GDLCODE, lifexp)

  geom <- geom %>%
    dplyr::select(GDLCode, geometry)


  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
  shdiyr <- priogrid::yearly_brick(shdi.full, variable = 'lifexp', raster.fun = 'mean')

}


# ~9 minutes run time



