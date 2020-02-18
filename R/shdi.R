
#' Generate Expected years schooling children aged 6 variable from SHDI
#'
#' Average Expected years schooling children aged 6 within each grid cell.
#'
#' @param path path to SHDI data.
#' @param variable one of c("msch", "esch", "lifexp", "lgnic", "shdi")
gen_shdi <- function(path, variable = "shdi"){
  shdi <- read.csv(file.path(path,"SHDI-Complete SD1.csv"))
  geom <- sf::read_sf(file.path(path,"GDL-SHDI-SHP-2.shp"))

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, GDLCODE, esch)

  geom <- geom %>%
    dplyr::select(GDLCode, geometry)


  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
  shdiyr <- priogrid::yearly_brick(shdi.full, variable = variable, raster.fun = 'mean')

}

gen_msch <- gen_shdi(path, variable == "msch")
gen_esch <- gen_shdi(path, variable == "esch")
gen_lifexp <- gen_shdi(path, variable == "lifexp")
gen_lgnic <- gen_shdi(path, variable == "lgnic")
