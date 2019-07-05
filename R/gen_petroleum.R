# library(devtools)
# devtools::load_all()
# library(priogrid)
# library(sf)
# library(lubridate)
# library(tidyverse)


# Create yearly petroleum presence dummy ----------------------------------

prio_petro_y <- function(input_file, output_file, ncores = 1){
  # Load data
  petroleum <- sf::st_read(input_file)
  petroleum <- sf:st_set_crs(petroleum, value = prio_crs())

  # Data prep
  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc = DISC, prod = PROD, geometry)

  petroleum$disc[which(petroleum$disc == -9999)] <- NA
  petroleum$prod[which(petroleum$prod == -9999)] <- NA

  petroleum$disc[which(petroleum$disc < 1946)] <- 1946
  petroleum$prod[which(petroleum$prod < 1946)] <- 1946

  # Create yearly dummy
  petroleum_y <- petroleum %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(prod) | !is.na(disc)) %>%
    dplyr::mutate(startyear = pmin(prod, disc, na.rm = TRUE), # earliest occurrance of discovery or start of production
                  endyear = 2003,
                  year = purrr::map2(startyear, endyear, `:`),
                  petroleum_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, petroleum_y, geometry)
}



# Create static petroleum presence dummy ----------------------------------
prio_petro_s <- function(input_file, output_file, ncores = 1){
  # Load data
  petroleum <- sf::st_read(input_file)
  petroleum <- sf:st_set_crs(petroleum, value = prio_crs())

  # Data prep
  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc = DISC, prod = PROD, geometry)

  petroleum$disc[which(petroleum$disc == -9999)] <- NA
  petroleum$prod[which(petroleum$prod == -9999)] <- NA

  petroleum$disc[which(petroleum$disc < 1946)] <- 1946
  petroleum$prod[which(petroleum$prod < 1946)] <- 1946

  # Create static dummy
  petroleum_s <- petroleum %>%
    dplyr::filter(is.na(prod) & is.na(disc)) %>%
    dplyr::mutate(petroleum_s = 1) %>%
    dplyr::select(gwno, petroleum_s, geometry)
}


