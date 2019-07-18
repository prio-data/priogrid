
# Create yearly petroleum presence dummy ----------------------------------
## Will be updated to return PRIO-GRID compatible raster.

#' Generate yearly petroleum presence dummy variable. 
#' 
#' @param petro_data PRIO Petroleum Dataset v. 1.2 onshore shapefile 


gen_petro_y <- function(petro_data){
  petroleum <- priogrid::prep_petro(petro_data)
  
  petroleum <- priogrid::yearly_dummy(data = petroleum, endyear = 2003) %>%
    dplyr::rename(petroleum_y = dummy)

}


# Create static petroleum presence dummy ----------------------------------
#' Generate static petroleum presence dummy variable
#' for records without known year of discovery or production start.
#' 
#' @param petro_data PRIO Petroleum Dataset v. 1.2 onshore shapefile 



gen_petro_s <- function(petro_data){
  petroleum <- priogrid::prep_petro(petro_data)
  
  petroleum <- priogrid::static_dummy(petroleum) %>%
    dplyr::rename(petroleum_s = dummy)
  
  petroleum_raster <- raster::rasterize(petroleum, priogrid::prio_blank_grid(),
                                        field = "petroleum_s", fun = "first")
  names(petroleum_raster) <- "petroleum_s"
  return(petroleum_raster)

}




# Petroleum data prep function --------------------------------------------

prep_petro <- function(petro_data){
  # Load data
  petroleum <- sf::st_read(petro_data)
  petroleum <- sf::st_set_crs(petroleum, value = priogrid::prio_crs())
  
  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc.year = DISC, prod.year = PROD, geometry)
  
  petroleum$disc.year <- priogrid::prio_NA(petroleum$disc.year, -9999)
  petroleum$prod.year <- priogrid::prio_NA(petroleum$prod.year, -9999)
  
  petroleum$disc.year <- priogrid::prio_earliest(petroleum$disc.year)
  petroleum$prod.year <- priogrid::prio_earliest(petroleum$prod.year)
  
  petroleum
}



