
#' Generate Expected years schooling children aged 6 variable from SHDI
#'
#' Average Expected years schooling children aged 6 within each grid cell.
#'
#' @param input_folder path to SHDI data.
#' @param variable one of c("msch", "esch", "lifexp", "lgnic", "shdi")
#'  @export
gen_shdi <- function(input_folder, variable = "shdi"){
  shdi <- read.csv(file.path(input_folder, "SHDI-Complete SD1.csv"))
  geom <- sf::read_sf(file.path(input_folder, "GDL-SHDI-SHP-2.shp"))

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, GDLCODE, !!variable)

  geom <- geom %>%
    dplyr::select(GDLCode, geometry)

  shdi <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")

  shdi <- priogrid::panel_to_pg(shdi, timevar = "year", variable = variable, need_aggregation = TRUE, missval = -1, fun = "mean")
  return(shdi)
}

gen_msch <- function(input_folder){ gen_shdi(input_folder, variable = "msch") }
gen_esch <- function(input_folder){ gen_shdi(input_folder, variable = "esch") }
gen_lifexp <- function(input_folder){ gen_shdi(input_folder, variable = "lifexp") }
gen_lgnic <- function(input_folder){ gen_shdi(input_folder, variable = "lgnic") }

