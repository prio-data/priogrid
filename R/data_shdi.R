
#' Generate Expected years schooling children aged 6 variable from SHDI
#'
#' Average Expected years schooling children aged 6 within each grid cell.
#'
#' @param path path to SHDI data.
#' @param variable one of c("msch", "esch", "lifexp", "lgnic", "shdi")
gen_shdi <- function(input_folder, variable = "shdi"){
  shdi <- read.csv(file.path(input_folder, "SHDI-Complete SD1.csv"))
  geom <- sf::st_read(file.path(input_folder, "GDL-SHDI-SHP-2.shp"))

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, GDLCODE, !!variable)

  geom <- geom %>%
    dplyr::select(GDLCode, geometry)


  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
  shdiyr <- priogrid::yearly_brick(shdi.full, variable = variable, raster.fun = 'mean')

  return(shdiyr)
}

gen_msch <- function(input_folder){ gen_shdi(input_folder, variable == "msch") }
gen_esch <- function(input_folder){ gen_shdi(input_folder, variable == "esch") }
gen_lifexp <- function(input_folder){ gen_shdi(input_folder, variable == "lifexp") }
gen_lgnic <- function(input_folder){ gen_shdi(input_folder, variable == "lgnic") }

