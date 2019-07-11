# Helper functions --------------------------------------------------------
## Note: these will be updated as they are not really functional 


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



# Yearly ------------------------------------------------------------------
#' Function to generate yearly gem presence dummy. 
#' Returns an sf object (will be updated to return raster layer with gid).
#' 
#' @param gemdata GEMDATA shapefile as available from http://www.paivilujala.com/gemdata.html.

gen_gems_y <- function(gem_data){
  # Load gem data
  gems <- sf::st_read(gem_data)
  
  # Data prep
  ## Missing data
  gems$DISC_Y <- priogrid::prio_NA(gems$DISC_Y, 0)
  gems$PRO_Y <- priogrid::prio_NA(gems$PRO_Y, 0)
  
  ## Set earliest start year of disc or prod to 1946
  gems$DISC_Y <- priogrid::prio_earliest(gems$DISC_Y)
  gems$PRO_Y <- priogrid::prio_earliest(gems$PRO_Y)

  
  # Create yearly presence dummy
  gems_y <- gems %>%
    dplyr::group_by(PRIMKEY) %>%
    dplyr::filter(!is.na(PRO_Y) | !is.na(DISC_Y)) %>%
    dplyr::mutate(gem_y = 1,
                  startyear = pmin(PRO_Y, DISC_Y, na.rm = TRUE),
                  year = priogrid::prio_year(startyear, 2004)) %>%
    dplyr::ungroup() %>%
    tidyr::unnest() %>%
    dplyr::select(gwno = COWCODE, year, gem_y, geometry)
  
  gems_y <- sf::st_transform(gems_y, crs = priogrid::prio_crs())
}



# Static ------------------------------------------------------------------
#' Function to generate static gem presence dummy for records without known discovery or start of production date.
#' Returns an sf object. 
#' 
#' @param gemdata GEMDATA shapefile as available from http://www.paivilujala.com/gemdata.html.


gen_gems_s <- function(gem_data){
  # Load gem data
  gems <- sf::st_read(gem_data)
  
  # Data prep
  ## Missing data
  gems$DISC_Y <- priogrid::prio_NA(gems$DISC_Y, 0)
  gems$PRO_Y <- priogrid::prio_NA(gems$PRO_Y, 0)
  
  ## Set earliest start year of disc or prod to 1946
  gems$DISC_Y <- priogrid::prio_earliest(gems$DISC_Y)
  gems$PRO_Y <- priogrid::prio_earliest(gems$PRO_Y)
  
  # Create static dummy
  gems <- gems %>%
    dplyr::filter(is.na(PRO_Y) & is.na(DISC_Y)) %>%
    dplyr::mutate(gem_s = 1) %>%
    dplyr::select(gwno = COWCODE, gem_s, geometry)
  
  # Harmonize with PRIO-GRID
  gems <- sf::st_transform(gems, crs = priogrid::prio_crs())
}
