# library(devtools)
# devtools::load_all()
# library(priogrid)
# library(sf)
# library(lubridate)
# library(tidyverse)



# Yearly diamond presence dummy -------------------------------------------

prio_diamonds_y <- function(input_file, output_file, ncores = 1){
  # Load diamond data
  diamonds <- sf::st_read(input_file)

  # Data prep
  diamonds$disc.year <- lubridate::year(diamonds$DISC)
  diamonds$PROD <- base::as.Date(diamonds$PROD, format = "%d/%m/%Y")
  diamonds$prod.year <- lubridate::year(diamonds$PROD)

  # Generate yearly presence dummy
  diamonds_y <- diamonds %>%
    dplyr::group_by(PRIMKEY) %>%
    dplyr::filter(!is.na(prod.year) | !is.na(disc.year)) %>%
    dplyr::mutate(diamprim_y = ifelse(DIAINFO == "P", yes = 1, no = NA),
                  diamsec_y = ifelse(DIAINFO == "S", yes = 1, no = NA),
                  startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  endyear = 2005) %>%
    filter(DIAINFO == "S" | DIAINFO == "P") %>%
    mutate(year = list(startyear:endyear)) %>% # Time series
    tidyr::unnest() %>%
    ungroup() %>%
    dplyr::select(gwno = COWCODE, year, diamsec_y, diamprim_y, geometry)

  # Harmonize with PRIO-GRID
  diamonds_y <- st_set_crs(diamonds_y, value = prio_crs())
}

## NB! Merge with PRIO-GRID cells with st_join. Not sure if the function should do that automatically


# Static diamond presence dummy for records without known year ------------

prio_diamonds_s <- function(input_file, output_file, ncores = 1){
  # Load diamond data
  diamonds <- sf::st_read(input_file)

  # Data prep
  diamonds$disc.year <- lubridate::year(diamonds$DISC)
  diamonds$PROD <- base::as.Date(diamonds$PROD, format = "%d/%m/%Y")
  diamonds$prod.year <- lubridate::year(diamonds$PROD)

  # Generate static presence dummy
  diamonds_s <- diamonds %>%
    dplyr::filter(is.na(prod.year) & is.na(disc.year)) %>%
    mutate(diamprim_s = ifelse(DIAINFO == "P", yes = 1, no = NA),
           diamsec_s = ifelse(DIAINFO == "S", yes = 1, no = NA)) %>%
    filter(DIAINFO == "S" | DIAINFO == "P") %>%
    dplyr::select(gwno = COWCODE, diamsec_s, diamprim_s, geometry)

  # Harmonize with PRIO-GRID
  diamonds_s <- st_set_crs(diamonds_s, value = prio_crs())
}

