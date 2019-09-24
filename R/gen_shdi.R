## SHDI

#' Generate SHDI variable.
#' Average human development index within each grid cell.
#'
#' @param path Path containing two data files 

gen_shdi <- function(path){
  shdi <- read.csv(file.path(path,"SHDI-Complete SD1.csv"))
  geom <- sf::read_sf(file.path(path,"GDL-SHDI-SHP-2.shp"))

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
