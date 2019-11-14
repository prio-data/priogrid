
#' Generate Mean years schooling population aged 25+ variable from SHDI
#'
#' Average Mean years schooling population aged 25+ within each grid cell.
#'
#' @param shdi_data SHDI .csv data.
#' @param shdi_sf SHDI MultiPolygon shapefile.



gen_msch <- function(path){
  shdi <- read.csv(file.path(path,"SHDI-Complete SD1.csv"))
  geom <- sf::read_sf(file.path(path,"GDL-SHDI-SHP-2.shp"))

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, GDLCODE, msch)

  geom <- geom %>%
    dplyr::select(GDLCode, geometry)


  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
  shdiyr <- priogrid::yearly_brick(shdi.full, variable = 'msch', raster.fun = 'mean')

}


# ~9 minutes run time
