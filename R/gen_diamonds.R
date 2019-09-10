
# Yearly ------------------------------------------------------------------


#' Generate yearly primary diamond presence dummy.
#'
#' @param diamond_data DIADATA shapefile from the PRIO Diamond Resources dataset.


gen_diamprim_y <- function(diamond_data){

  diamonds <- priogrid::prep_diamonds(diamond_data)

  diamonds <- priogrid::yearly_dummy(data = diamonds, endyear = 2005) %>%
    dplyr::rename(diamprim_y = diamprim)

  diamonds <- priogrid::yearly_brick(diamonds, variable = "diamprim_y", raster.fun = "first")

}


#' Generate yearly secondary diamond presence dummy.
#'
#' @param diamond_data DIADATA shapefile from the PRIO Diamond Resources dataset.

gen_diamsec_y <- function(diamond_data){

  diamonds <- priogrid::prep_diamonds(diamond_data)

  diamonds <- priogrid::yearly_dummy(data = diamonds, endyear = 2005) %>%
    dplyr::rename(diamsec_y = diamsec)

  diamonds <- priogrid::yearly_brick(diamonds, variable = "diamsec_y", raster.fun = "first")

}



# Static ------------------------------------------------------------------


#' Generate static diamond presence dummy
#' including records without known start year.
#'
#' @param diamond_data DIADATA shapefile from the PRIO Diamond Resources dataset.

gen_diamonds_s <- function(diamond_data){
  diamonds <- priogrid::prep_diamonds(diamond_data)

  diamonds <- priogrid::static_dummy(diamonds) %>%
    dplyr::rename(diamsec_s = diamsec, diamprim_s = diamprim)

  # Rasterize
  diamprim_s <- raster::rasterize(diamonds, priogrid::prio_blank_grid(),
                                  field = "diamprim_s", fun = "count")

  diamsec_s <- raster::rasterize(diamonds, priogrid::prio_blank_grid(),
                                 field = "diamsec_s", fun = "count")

  brick <- raster::brick(c(diamprim_s, diamsec_s))
  names(brick) <- c("diamprim_s", "diamsec_s")
  return(brick)
}




# Diamonds data prep function ---------------------------------------------

prep_diamonds <- function(diamond_data){

  diamonds <- sf::st_read(diamond_data)
  diamonds <- sf::st_transform(diamonds, crs = priogrid::prio_crs())

  diamonds$disc.year <- lubridate::year(diamonds$DISC)
  diamonds$PROD <- base::as.Date(diamonds$PROD, format = "%d/%m/%Y")
  diamonds$prod.year <- lubridate::year(diamonds$PROD)

  diamonds <- diamonds %>%
    dplyr::select(gwno = COWCODE, id = PRIMKEY, disc.year, prod.year, DIAINFO, geometry) %>%
    dplyr::mutate(diamprim = ifelse(DIAINFO == "P", yes = 1, no = NA),
                  diamsec = ifelse(DIAINFO == "S", yes = 1, no = NA)) %>%
    dplyr::filter(diamprim == 1 | diamsec == 1)

  diamonds
}

