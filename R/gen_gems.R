library(devtools)
devtools::load_all()
library(priogrid)
library(sf)
library(lubridate)
library(tidyverse)
library(spex)


# Yearly gem presence dummy -----------------------------------------------

prio_gems_y <- function(input_file, output_file, ncores = 1){
  # Load gem data
  gems <- sf::st_read(input_file)

  # Data prep
  ## Missing data
  gems$DISC_Y[which(gems$DISC_Y == 0)] <- NA
  gems$PRO_Y[which(gems$PRO_Y == 0)] <- NA

  ## Set earliest start year of disc or prod to 1946
  gems$DISC_Y[which(gems$DISC_Y < 1946)] <- 1946
  gems$PRO_Y[which(gems$PRO_Y < 1946)] <- 1946

  # Create yearly presence dummy
  gems_y <- gems %>%
    dplyr::group_by(PRIMKEY) %>%
    dplyr::filter(!is.na(PRO_Y) | !is.na(DISC_Y)) %>%
    dplyr::mutate(gem_y = 1,
           startyear = pmin(PRO_Y, DISC_Y, na.rm = TRUE),
           endyear = 2004,
           year = list(startyear:endyear)) %>%
    dplyr::ungroup() %>%
    tidyr::unnest() %>%
    dplyr::select(gwno = COWCODE, year, gem_y, geometry)
}

# Overlay with PRIO-GRID cells (poly) using st_join


# Static gem presence dummy -----------------------------------------------

prio_gems_s <- function(input_file, output_file, ncores = 1){
  # Load gem data
  gems <- sf::st_read(input_file)

  # Data prep
  ## Missing data
  gems$DISC_Y[which(gems$DISC_Y == 0)] <- NA
  gems$PRO_Y[which(gems$PRO_Y == 0)] <- NA

  ## Set earliest start year of disc or prod to 1946
  gems$DISC_Y[which(gems$DISC_Y < 1946)] <- 1946
  gems$PRO_Y[which(gems$PRO_Y < 1946)] <- 1946

  # Create static dummy
  gems_y <- gems %>%
    dplyr::filter(is.na(PRO_Y) & is.na(DISC_Y)) %>%
    dplyr::mutate(gem_s = 1) %>%
    dplyr::select(gwno = COWCODE, gem_s, geometry)

  # Harmonize with PRIO-GRID
  gems_y <- sf::st_as_sf(gems_y, value = prio_crs())
}

