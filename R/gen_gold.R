
## Create three separate gold presence dummy variables
## based on whether deposits are lootable, semi-lootable or non-lootable.


# Helper functions --------------------------------------------------------

prio_year <- function(startyear, endyear){
  purrr::map2(startyear, endyear, `:`)
}


prio_earliest <- function(x){
  x[x < 1946] <- 1946
  x
}


prio_NA <- function(x, na_value){
  x[x == na_value] <- NA
  x
}




# Lootable yearly dummy (goldplacer_y) ------------------------------------
#' Generate yearly dummy identifying lootable placer gold deposits.
#' 
#' @param golddata_l GOLDATA 1.2 v lootable gold shapefile 
#' as available from https://www.researchgate.net/publication/281849073_GOLDATA_12_v. 

gen_goldplacer_y <- function(golddata_l){

  # Load data
  gold_lootable <- sf::st_read(gold_data)
  gold_lootable <- sf::st_set_crs(gold_lootable, value = priogrid::prio_crs())

  gold_lootable <- gold_lootable %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_lootable$disc.year <- priogrid::prio_NA(gold_lootable$disc.year, 9999)
  gold_lootable$prod.year <- priogrid::prio_NA(gold_lootable$prod.year, 9999)

  # Create yearly placer gold presence dummy for records with discovery or production year
  gold_lootable_y <- gold_lootable %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  year = priogrid::prio_year(startyear, 2012),
                  goldplacer_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, year, goldplacer_y, geometry)

}



# Semi-lootable yearly dummy (goldsurface_y) ------------------------------
#' Generate yearly dummy identifying semi-lootable surface gold deposits.
#' 
#' @param golddata_s GOLDATA 1.2 v semi-lootable gold shapefile
#' as available from https://www.researchgate.net/publication/281849073_GOLDATA_12_v. 


gen_goldsurface_y <- function(golddata_s){
  # Load data
  gold_semiloot <- sf::st_read(golddata_s)
  gold_semiloot <- sf::st_set_crs(gold_semiloot, value = priogrid::prio_crs())

  gold_semiloot <- gold_semiloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_semiloot$disc.year <- priogrid::prio_NA(gold_semiloot$disc.year, 9999)
  gold_semiloot$prod.year <- priogrid::prio_NA(gold_semiloot$prod.year, 9999)

  # Create yearly surface gold presence dummy for records with discovery or production year
  gold_semiloot_y <- gold_semiloot %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  year = priogrid::prio_year(startyear, 2012),
                  goldsurface_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, year, goldsurface_y, geometry)

}




# Non-lootable yearly dummy (goldvein_y) ----------------------------------
#' Generate yearly dummy identifying non-lootable vein gold deposits.
#' 
#' @param golddata_nl GOLDATA 1.2 v non-lootable gold shapefile
#' as available from https://www.researchgate.net/publication/281849073_GOLDATA_12_v. 


gen_goldvein_y <- function(golddata_nl){
  # Load data
  gold_nonloot <- sf::st_read(golddata_nl)
  gold_nonloot <- sf::st_set_crs(gold_nonloot, value = priogrid::prio_crs())

  gold_nonloot <- gold_nonloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_semiloot$disc.year <- priogrid::prio_NA(gold_semiloot$disc.year, 9999)
  gold_semiloot$prod.year <- priogrid::prio_NA(gold_semiloot$prod.year, 9999)

  # Create yearly vein gold presence dummy for records with discovery or production year
  gold_nonloot_y <- gold_nonloot %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  year = priogrid::prio_year(startyear, 2012),
                  goldvein_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, year, goldvein_y, geometry)

}



# Lootable static dummy (goldplacer_s) ------------------------------------
#' Generate static dummy identifying lootable placer gold deposits
#' for records without known year of discovery or production start.
#' 
#' @param golddata_l GOLDATA 1.2 v lootable gold shapefile 
#' as available from https://www.researchgate.net/publication/281849073_GOLDATA_12_v. 


gen_goldplacer_s <- function(golddata_l){
  # Load data
  gold_lootable <- sf::st_read(golddata_l)
  gold_lootable <- sf::st_set_crs(gold_lootable, value = priogrid::prio_crs())

  gold_lootable <- gold_lootable %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_semiloot$disc.year <- priogrid::prio_NA(gold_semiloot$disc.year, 9999)
  gold_semiloot$prod.year <- priogrid::prio_NA(gold_semiloot$prod.year, 9999)
  
  # Create static gold presence dummy for records without discovery or production year
  gold_lootable_s <- gold_lootable %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(goldplacer_s = 1) %>%
    dplyr::select(gwno, goldplacer_s, geometry)
}



# Semi-lootable static dummy (goldsurface_s) ------------------------------
#' Generate static dummy identifying semi-lootable surface gold deposits
#' for records without known year of discovery or production start.
#' 
#' @param golddata_s GOLDATA 1.2 v semi-lootable gold shapefile
#' as available from https://www.researchgate.net/publication/281849073_GOLDATA_12_v. 



gen_goldsurface_s <- function(golddata_s){
  # Load data
  gold_semiloot <- sf::st_read(golddata_s)
  gold_semiloot <- sf::st_set_crs(gold_semiloot, value = prio_crs())

  gold_semiloot <- gold_semiloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_semiloot$disc.year <- priogrid::prio_NA(gold_semiloot$disc.year, 9999)
  gold_semiloot$prod.year <- priogrid::prio_NA(gold_semiloot$prod.year, 9999)
  
  # Create static gold presence dummy for records without discovery or production year
  gold_semiloot_s <- gold_semiloot %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(goldsurface_s = 1) %>%
    dplyr::select(gwno, goldsurface_s, geometry)

}



# Non-lootable static dummy (goldvein_s) ----------------------------------
#' Generate static dummy identifying non-lootable vein gold deposits,
#' for records without known year of discovery or production start.
#' 
#' @param golddata_nl GOLDATA 1.2 v non-lootable gold shapefile
#' as available from https://www.researchgate.net/publication/281849073_GOLDATA_12_v. 


gen_goldvein_s <- function(golddata_nl){
  # Load data
  gold_nonloot <- sf::st_read(golddata_nl)
  gold_nonloot <- sf::st_set_crs(gold_nonloot, value = priogrid::prio_crs())

  gold_nonloot <- gold_nonloot %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  # Data prep
  gold_nonloot$disc.year <- priogrid::prio_NA(gold_nonloot$disc.year, 9999)
  gold_nonloot$prod.year <- priogrid::prio_NA(gold_nonloot$prod.year, 9999)
  
  # Create static gold presence dummy for records without discovery or production year
  gold_nonloot_s <- gold_nonloot %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(goldvein_s = 1) %>%
    dplyr::select(gwno, goldvein_s, geometry)
}
