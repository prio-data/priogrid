
#' Generate GNI per capita variable.
#'
#' Average Log Gross National Income per capita PPP (2011 USD) within each grid cell.
#'
#' @param shdi_data SHDI .csv data.
#' @param shdi_sf SHDI MultiPolygon shapefile.



gen_lgnic <- function(shdi_data, shdi_sf){

  shdi <- read.csv(shdi_data)

  geom <- sf::read_sf(shdi_sf)

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, GDLCODE, lgnic)

  geom <- geom %>%
    dplyr::select(GDLCode, geometry)


  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
  shdiyr <- priogrid::yearly_brick(shdi.full, variable = 'lgnic', raster.fun = 'mean')

}





