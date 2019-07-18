# Yearly ------------------------------------------------------------------
## Returns an sf object (will be updated to return raster layer with gid).

#' Generate yearly gem dummy.
#' Function to generate yearly gem presence dummy for records with known discovery or start of production year. 
#' 
#' @param gemdata GEMDATA shapefile.

gen_gems_y <- function(gem_data){
  gems <- priogrid::prep_gems(gem_data)
  
  # Create yearly presence dummy
  gems <- priogrid::yearly_dummy(gems, endyear = 2004) %>%
    dplyr::rename(gems_y = dummy)
  
}



# Static ------------------------------------------------------------------
#' Generate static gem dummy.
#'  
#' Function to generate static gem presence dummy for records without known discovery or start of production year.
#'  
#' @param gemdata GEMDATA shapefile.
#' 
#' @return RasterLayer.


gen_gems_s <- function(gem_data){
  gems <- priogrid::prep_gems(gem_data)
  
  gems <- priogrid::static_dummy(gems) %>%
    dplyr::rename(gem_s = dummy)
  
  gems_raster <- raster::rasterize(gems, priogrid::prio_blank_grid(), field = "gem_s", fun = "first")
  names(gems_raster) <- "gem_s"
  return(gems_raster)
}





# Gem data prep function --------------------------------------------------

prep_gems <- function(gem_data){
  gems <- sf::st_read(gem_data)
  gems <- sf::st_transform(gems, crs = priogrid::prio_crs())
  
  gems$DISC_Y <- priogrid::prio_NA(gems$DISC_Y, 0)
  gems$PRO_Y <- priogrid::prio_NA(gems$PRO_Y, 0)
  
  gems$DISC_Y <- priogrid::prio_earliest(gems$DISC_Y)
  gems$PRO_Y <- priogrid::prio_earliest(gems$PRO_Y)
  
  gems <- gems %>%
    dplyr::select(gwno = COWCODE, id = PRIMKEY, disc.year = DISC_Y, prod.year = PRO_Y, geometry)
  
}
