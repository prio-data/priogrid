
#' Generate GNI per capita variable.
#'
#' Average Log Gross National Income per capita PPP (2011 USD) within each grid cell.
#'
#' @param shdi_data SHDI .csv data.
#' @param shdi_sf SHDI MultiPolygon shapefile.



gen_lgnic <- function(path){
   shdi <- read.csv(file.path(path,"SHDI-Complete SD1.csv"))
   geom <- sf::read_sf(file.path(path,"GDL-SHDI-SHP-2.shp"))

   shdi <- shdi %>%
      dplyr::filter(level == "Subnat") %>%
      dplyr::select(year, GDLCODE, lgnic)

   geom <- geom %>%
      dplyr::select(GDLCode, geometry)


   shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
   shdiyr <- priogrid::yearly_brick(shdi.full, variable = 'lgnic', raster.fun = 'mean')

}


# ~9 minutes run time


