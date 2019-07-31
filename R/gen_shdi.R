## SHDI

#' Generate SHDI variable.
#' Average human development index within each grid cell.
#'
#' @param shdi_data SHDI csv data.
#' @param shdi_sf SHDI shapefile.

gen_shdi <- function(shdi_data, shdi_sf){

  shdi <- read.csv(shdi_data)

  geom <- sf::read_sf(shdi_sf)

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, iso_code, GDLCODE, shdi)

  geom <- geom %>%
    dplyr::select(GDLCode, iso_code, geometry)


  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")

  shdiyr <- priogrid::yearly_brick(shdi.full, variable = 'shdi', raster.fun = 'mean')
  return(shdiyr)
}

# ~13 mins
