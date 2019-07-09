
# Functions used to create the gwcode variable
# should probably me put into a more sensibly-
# named file eventually.

#' Landgrid
#' 
#' Generate a raster that only contains gridcells on 
#' or close to landmasses. Hopefully improves performance 
#' of some functions.
#' @param cshapes An sf object with cshapes 

landgrid <- function(cshapes, blank = NULL){
   if(is.null(blank)){
   blank <- priogrid::prio_blank_grid() 
   }

   simpleworld <- sf::st_simplify(cshapes, dTolerance = 1.0) %>%
      sf::st_buffer(1) %>%
      sf::st_simplify(dTolerance = 1.5) %>%
      sf::st_union() 

   dummy <- tibble::tibble(x = 1)
   dummy <- sf::st_sf(dummy, geometry = simpleworld)

   raster::mask(blank,dummy)
}

#' America / Eurasia raster
#'
#' Returns a blank raster dummy splitting the world into two hemispheres.
#' 
#' @return An america / eurasia raster 
prio_america_eurasia <- function(blank = NULL){
   if(is.null(blank)){
   blank <- prio_blank_grid()
   }

   latitudes <- raster::xFromCell(blank, 1:length(blank))

   amasia <- ifelse(latitudes > prio_midatlantic,
                    'eurasia',
                    'americas') %>% as.factor()

   raster::values(blank) <- amasia
   blank 
}


