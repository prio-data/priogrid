## Create three separate gold presence dummy variables
## based on whether deposits are lootable, semi-lootable or non-lootable.

## TODO Rasterize yearly variables and write function for stacking these (yearly layers?)


# Lootable yearly dummy (goldplacer_y) ------------------------------------
#' Generate yearly dummy identifying lootable placer gold deposits.
#' 
#' @param golddata_l GOLDATA 1.2 v lootable gold shapefile.

gen_goldplacer_y <- function(golddata_l){
  gold_lootable <- priogrid::prep_gold(golddata_l)

  # Create yearly placer gold presence dummy for records with discovery or production year
  gold_lootable <- priogrid::yearly_dummy(gold_lootable) %>%
    dplyr::rename(goldplacer_y = dummy)

}



# Semi-lootable yearly dummy (goldsurface_y) ------------------------------
#' Generate yearly dummy identifying semi-lootable surface gold deposits.
#' 
#' @param golddata_s GOLDATA 1.2 v semi-lootable gold shapefile.


gen_goldsurface_y <- function(golddata_s){
  gold_semiloot <- priogrid::prep_gold(golddata_s)

  # Create yearly surface gold presence dummy for records with discovery or production year
  gold_semiloot <- priogrid::yearly_dummy(gold_semiloot) %>%
    dplyr::rename(goldsurface_y = dummy)

}


# Non-lootable yearly dummy (goldvein_y) ----------------------------------
#' Generate yearly dummy identifying non-lootable vein gold deposits.
#' 
#' @param golddata_nl GOLDATA 1.2 v non-lootable gold shapefile.


gen_goldvein_y <- function(golddata_nl){
  gold_nonloot <- priogrid::prep_gold(golddata_nl)

  # Create yearly vein gold presence dummy for records with discovery or production year
  gold_nonloot <- priogrid::yearly_dummy(gold_nonloot) %>%
    dplyr::rename(goldvein_y = dummy)

}



# Static ------------------------------------------------------------------

#' Generate static gold dummy variables.
#' 
#' Function to generate static gold presence variables for records without known discovery or production year.
#' 
#' @param golddata_l GOLDATA 1.2 v lootable gold shapefile.
#' @param golddata_s GOLDATA 1.2 v semi-lootable gold shapefile.
#' @param golddata_nl GOLDATA 1.2 v non-lootable gold shapefile.
#' 
#' @return PRIO-GRID compatible RasterStack with one layer for each variable. 


gen_gold_s <- function(golddata_l, golddata_s, golddata_nl){
  goldplacer_s <- priogrid::gen_goldplacer_s(golddata_l)
  
  goldsurface_s <- priogrid::gen_goldsurface_s(golddata_s)
  
  goldvein_s <- priogrid::gen_goldvein_s(golddata_nl)
  
  # Combine RasterLayers
  gold_raster <- raster::stack(c(goldplacer_s, goldsurface_s, goldvein_s))
  names(gold_raster) <- c("goldplacer_s", "goldsurface_s", "goldvein_s")
  return(gold_raster)
}





# Static functions --------------------------------------------------------


# Lootable static dummy (goldplacer_s) ------------------------------------
gen_goldplacer_s <- function(golddata_l){

  gold_lootable <- priogrid::prep_gold(golddata_l)
  
  gold_lootable <- priogrid::static_dummy(gold_lootable) %>%
    dplyr::rename(goldplacer_s = dummy)
  
  loot_raster <- raster::rasterize(gold_lootable, priogrid::prio_blank_grid(), field = "goldplacer_s", fun = "first")
  return(loot_raster)
  
}


# Semi-lootable static dummy (goldsurface_s) ------------------------------
gen_goldsurface_s <- function(golddata_s){

  gold_semiloot <- priogrid::prep_gold(golddata_s)
  
  gold_semiloot <- priogrid::static_dummy(gold_semiloot) %>%
    dplyr::rename(goldsurface_s = dummy)
  
  semi_raster <- raster::rasterize(gold_semiloot, priogrid::prio_blank_grid(), field = "goldsurface_s", fun = "first")
  return(semi_raster)
  
}



# Non-lootable static dummy (goldvein_s) ----------------------------------
gen_goldvein_s <- function(golddata_nl){
  
  gold_nonloot <- priogrid::prep_gold(golddata_nl)

  gold_nonloot <- priogrid::static_dummy(gold_nonloot) %>%
    dplyr::rename(goldvein_s = dummy)
  
  non_raster <- raster::rasterize(gold_nonloot, priogrid::prio_blank_grid(), field = "goldvein_s", fun = "first")
  return(non_raster)
  
}


# Gold data prep function -------------------------------------------------

prep_gold <- function(golddata){
  gold <- sf::st_read(golddata)
  gold <- sf::st_set_crs(gold, value = priogrid::prio_crs())
  
  gold <- gold %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)
  
  gold$disc.year <- priogrid::prio_NA(gold$disc.year, 9999)
  gold$prod.year <- priogrid::prio_NA(gold$prod.year, 9999)
  
  gold
  
}


