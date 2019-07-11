
# Helper functions --------------------------------------------------------
prio_year <- function(startyear, endyear){
  purrr::map2(startyear, endyear, `:`)
}


# Yearly ------------------------------------------------------------------


#' Generate yearly diamond presence dummy.
#'
#' @param diamond_data DIADATA shapefile from the PRIO Diamond Resources dataset.


gen_diamonds_y <- function(diamond_data){
  # Load diamond data
  diamonds <- sf::st_read(diamond_data)

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
                  year = priogrid::prio_year(startyear, 2005)) %>%
    filter(DIAINFO == "S" | DIAINFO == "P") %>%
    tidyr::unnest() %>%
    ungroup() %>%
    dplyr::select(gwno = COWCODE, year, diamsec_y, diamprim_y, geometry)

  # Harmonize with PRIO-GRID
  diamonds_y <- sf::st_transform(diamonds_y, crs = priogrid::prio_crs())
}



# Static ------------------------------------------------------------------


#' Generate static diamond presence dummy
#' including records without known start year.
#'
#' @param diamond_data DIADATA shapefile from the PRIO Diamond Resources dataset.

prio_diamonds_s <- function(diamond_data){
  # Load diamond data
  diamonds <- sf::st_read(diamond_data)

  # Data prep
  diamonds$disc.year <- lubridate::year(diamonds$DISC)
  diamonds$PROD <- base::as.Date(diamonds$PROD, format = "%d/%m/%Y")
  diamonds$prod.year <- lubridate::year(diamonds$PROD)

  # Generate static presence dummy
  diamonds_s <- diamonds %>%
    dplyr::filter(is.na(prod.year) & is.na(disc.year)) %>%
    dplyr::mutate(diamprim_s = ifelse(DIAINFO == "P", yes = 1, no = NA),
           diamsec_s = ifelse(DIAINFO == "S", yes = 1, no = NA)) %>%
    dplyr::filter(DIAINFO == "S" | DIAINFO == "P") %>%
    dplyr::select(gwno = COWCODE, diamsec_s, diamprim_s, geometry)

  # Harmonize with PRIO-GRID
  diamonds_s <- st_set_crs(diamonds_s, value = priogrid::prio_crs())
}

