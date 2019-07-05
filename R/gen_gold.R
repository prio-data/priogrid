# library(devtools)
# devtools::load_all()
# library(priogrid)
# library(sf)
# library(lubridate)
# library(tidyverse)

## Create three separate gold presence dummy variables
## based on whether deposits are lootable, semi-lootable or non-lootable.


# Lootable yearly dummy (goldplacer_y) ------------------------------------

goldplacer_y <- function(input_file, output_file, ncores = 1){

  # Load data
  gold_lootable <- sf::st_read(input_file)
  gold_lootable <- sf::st_set_crs(gold_lootable, value = prio_crs())

  gold_lootable <- gold_lootable %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_lootable$disc.year[which(gold_lootable$disc.year == 9999)] <- NA
  gold_lootable$prod.year[which(gold_lootable$prod.year == 9999)] <- NA

  # Create yearly placer gold presence dummy for records with discovery or production year
  gold_lootable_y <- gold_lootable %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  endyear = 2012,
                  year = purrr::map2(startyear, endyear, `:`),
                  goldplacer_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, year, goldplacer_y, geometry)

}



# Semi-lootable yearly dummy (goldsurface_y) ------------------------------

goldsurface_y <- function(input_file, output_file, ncores = 1){
  # Load data
  gold_semiloot <- sf::st_read("GOLDATA1.2v/dGOLD_S/dGOLD_S.shp")
  gold_semiloot <- sf::st_set_crs(gold_semiloot, value = prio_crs())

  gold_semiloot <- gold_semiloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_semiloot$disc.year[which(gold_semiloot$disc.year == 9999)] <- NA
  gold_semiloot$prod.year[which(gold_semiloot$prod.year == 9999)] <- NA

  # Create yearly surface gold presence dummy for records with discovery or production year
  gold_semiloot_y <- gold_semiloot %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  endyear = 2012,
                  year = purrr::map2(startyear, endyear, `:`),
                  goldsurface_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, year, goldsurface_y, geometry)

}




# Non-lootable yearly dummy (goldvein_y) ----------------------------------

goldvein_y <- function(input_file, output_file, ncores = 1){
  # Load data
  gold_nonloot <- sf::st_read("GOLDATA1.2v/dGOLD_NL/dGOLD_NL.shp")
  gold_nonloot <- sf::st_set_crs(gold_nonloot, value = prio_crs())

  gold_nonloot <- gold_nonloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_nonloot$disc.year[which(gold_nonloot$disc.year == 9999)] <- NA
  gold_nonloot$prod.year[which(gold_nonloot$prod.year == 9999)] <- NA

  # Create yearly vein gold presence dummy for records with discovery or production year
  gold_nonloot_y <- gold_nonloot %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  endyear = 2012,
                  year = purrr::map2(startyear, endyear, `:`),
                  goldvein_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, year, goldvein_y, geometry)

}




# Lootable static dummy (goldplacer_s) ------------------------------------

goldplacer_s <- function(input_file, output_file, ncores = 1){
  # Load data
  gold_lootable <- sf::st_read("GOLDATA1.2v/dGOLD_L/dGOLD_L.shp")
  gold_lootable <- sf::st_set_crs(gold_lootable, value = prio_crs())

  gold_lootable <- gold_lootable %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_lootable$disc.year[which(gold_lootable$disc.year == 9999)] <- NA
  gold_lootable$prod.year[which(gold_lootable$prod.year == 9999)] <- NA

  # Create static gold presence dummy for records without discovery or production year
  gold_lootable_s <- gold_lootable %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(goldplacer_s = 1) %>%
    dplyr::select(gwno, goldplacer_s, geometry)
}



# Semi-lootable static dummy (goldsurface_s) ------------------------------

goldsurface_s <- function(input_file, output_file, ncores = 1){
  # Load data
  gold_semiloot <- sf::st_read("GOLDATA1.2v/dGOLD_S/dGOLD_S.shp")
  gold_semiloot <- sf::st_set_crs(gold_semiloot, value = prio_crs())

  gold_semiloot <- gold_semiloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_semiloot$disc.year[which(gold_semiloot$disc.year == 9999)] <- NA
  gold_semiloot$prod.year[which(gold_semiloot$prod.year == 9999)] <- NA

  # Create static gold presence dummy for records without discovery or production year
  gold_semiloot_s <- gold_semiloot %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(goldsurface_s = 1) %>%
    dplyr::select(gwno, goldsurface_s, geometry)

}



# Non-lootable static dummy (goldvein_s) ----------------------------------

goldvein_s <- function(input_file, output_file, ncores = 1){
  # Load data
  gold_nonloot <- sf::st_read("GOLDATA1.2v/dGOLD_NL/dGOLD_NL.shp")
  gold_nonloot <- sf::st_set_crs(gold_nonloot, value = prio_crs())

  gold_nonloot <- gold_nonloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_nonloot$disc.year[which(gold_nonloot$disc.year == 9999)] <- NA
  gold_nonloot$prod.year[which(gold_nonloot$prod.year == 9999)] <- NA

  # Create static gold presence dummy for records without discovery or production year
  gold_nonloot_s <- gold_nonloot %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(goldvein_s = 1) %>%
    dplyr::select(gwno, goldvein_s, geometry)
}
