# Resource data


prep_diamonds <- function(diamond_data){

  diamonds <- sf::st_read(diamond_data, quiet = TRUE)
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

#' Generate drug dummy.
#'
#' Generate yearly dummy identifying ongoing large-scale drug cultivation
#' (coca bush, opium poppy and/or cannabis).
#'
#' @param cannabis_data Cannabis shapefile from the DRUGDATA dataset.
#' @param coca_data Coca bush shapefile from the DRUGDATA dataset.
#' @param opium_data Opium poppy shapefile from the DRUGDATA dataset.
prep_drugs <- function(cannabis_data, coca_data, opium_data){
  # Load three drug datasets
  cannabis <- sf::st_read(cannabis_data, stringsAsFactors = FALSE, quiet = TRUE)
  coca <- sf::st_read(coca_data, stringsAsFactors = FALSE, quiet = TRUE)
  opium <- sf::st_read(opium_data, stringsAsFactors = FALSE, quiet = TRUE)

  cleanup <- function(data, variables){
    data <- data %>%
      janitor::clean_names() %>%
      dplyr::select(variables)
  }
  variables <- c("id", "country", "begin", "end", "geometry")

  drugs <- rbind(cleanup(cannabis, variables),
                 cleanup(coca, variables),
                 cleanup(opium, variables))

  drugs$end[which(is.na(drugs$end))] <- 2002

  drugs$begin <- priogrid::prio_earliest(drugs$begin)

  drugs <- sf::st_set_crs(drugs, value = priogrid::prio_crs())

  drugs

}

gen_drug_y <- function(data_dir){
  files <- c("CANNABIS.shp","COCA BUSH.shp","OPIUM POPPY.shp")
  paths <- as.list(file.path(data_dir,files))
  drugs <- do.call(priogrid::prep_drugs, paths)

  drugs <- drugs %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(year = priogrid::prio_year(begin, end),
                  drug_y = 1) %>%
    tidyr::unnest(year) %>%
    dplyr::ungroup()

  drugs <- sf::st_cast(drugs, "MULTIPOLYGON")

  drugs <- priogrid::yearly_brick(drugs, variable = "drug_y", raster.fun = "first")

}

#' Generate yearly gem dummy.
#' Function to generate yearly gem presence dummy for records with known discovery or start of production year.
#'
#' @param gemdata GEMDATA shapefile.
gen_gem_y <- function(gem_data){
  gems <- priogrid::prep_gems(gem_data)

  # Create yearly presence dummy
  gems <- priogrid::yearly_dummy(gems, endyear = 2004) %>%
    dplyr::rename(gem_y = dummy)

  # Yearly Rasterbrick

  gems <- priogrid::yearly_brick(gems, variable = "gem_y", raster.fun = "first")
}

#' Generate static gem dummy.
#'
#' Function to generate static gem presence dummy for records without known discovery or start of production year.
#'
#' @param gemdata GEMDATA shapefile.
#'
#' @return RasterLayer.
gen_gem_s <- function(gem_data){
  gems <- priogrid::prep_gems(gem_data)

  gems <- priogrid::static_dummy(gems) %>%
    dplyr::rename(gem_s = dummy)

  gems_raster <- raster::rasterize(gems, priogrid::prio_blank_grid(), field = "gem_s", fun = "first")
  names(gems_raster) <- "gem_s"
  return(gems_raster)
}

prep_gems <- function(gem_data){
  gems <- sf::st_read(gem_data, quiet = TRUE)
  gems <- sf::st_transform(gems, crs = priogrid::prio_crs())

  gems$DISC_Y <- priogrid::prio_NA(gems$DISC_Y, 0)
  gems$PRO_Y <- priogrid::prio_NA(gems$PRO_Y, 0)

  gems$DISC_Y <- priogrid::prio_earliest(gems$DISC_Y)
  gems$PRO_Y <- priogrid::prio_earliest(gems$PRO_Y)

  gems <- gems %>%
    dplyr::select(gwno = COWCODE, id = PRIMKEY, disc.year = DISC_Y, prod.year = PRO_Y, geometry)

}

prep_gold <- function(golddata){
  gold <- sf::st_read(golddata, quiet = TRUE)
  gold <- sf::st_set_crs(gold, value = priogrid::prio_crs())

  gold <- gold %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  gold$disc.year <- priogrid::prio_NA(gold$disc.year, 9999)
  gold$prod.year <- priogrid::prio_NA(gold$prod.year, 9999)

  gold

}

#' Generate yearly dummy identifying lootable placer gold deposits.
#'
#' @param golddata_l GOLDATA 1.2 v lootable gold shapefile.
gen_goldplacer_y <- function(golddata_l){
  gold_lootable <- priogrid::prep_gold(golddata_l)

  gold_lootable <- priogrid::yearly_dummy(gold_lootable, endyear = 2012) %>%
    dplyr::rename(goldplacer_y = dummy)

  gold_lootable <- priogrid::yearly_brick(gold_lootable,
                                          variable = "goldplacer_y", raster.fun = "first")

}


#' Generate yearly dummy identifying semi-lootable surface gold deposits.
#'
#' @param golddata_s GOLDATA 1.2 v semi-lootable gold shapefile.
gen_goldsurface_y <- function(golddata_s){
  gold_semiloot <- priogrid::prep_gold(golddata_s)

  gold_semiloot <- priogrid::yearly_dummy(gold_semiloot, endyear = 2012) %>%
    dplyr::rename(goldsurface_y = dummy)

  gold_semiloot <- priogrid::yearly_brick(gold_semiloot,
                                          variable = "goldsurface_y", raster.fun = "first")

}


#' Generate yearly dummy identifying non-lootable vein gold deposits.
#'
#' @param golddata_nl GOLDATA 1.2 v non-lootable gold shapefile.
gen_goldvein_y <- function(golddata_nl){
  gold_nonloot <- priogrid::prep_gold(golddata_nl)

  gold_nonloot <- priogrid::yearly_dummy(gold_nonloot, 2012) %>%
    dplyr::rename(goldvein_y = dummy)

  gold_nonloot <- priogrid::yearly_brick(gold_nonloot,
                                         variable = "goldvein_y", raster.fun = "first")

}

gen_goldplacer_s <- function(golddata_l){

  gold_lootable <- priogrid::prep_gold(golddata_l)

  gold_lootable <- priogrid::static_dummy(gold_lootable) %>%
    dplyr::rename(goldplacer_s = dummy)

  loot_raster <- raster::rasterize(gold_lootable, priogrid::prio_blank_grid(),
                                   field = "goldplacer_s", fun = "first")
  return(loot_raster)

}

gen_goldsurface_s <- function(golddata_s){

  gold_semiloot <- priogrid::prep_gold(golddata_s)

  gold_semiloot <- priogrid::static_dummy(gold_semiloot) %>%
    dplyr::rename(goldsurface_s = dummy)

  semi_raster <- raster::rasterize(gold_semiloot, priogrid::prio_blank_grid(),
                                   field = "goldsurface_s", fun = "first")
  return(semi_raster)

}

gen_goldvein_s <- function(golddata_nl){

  gold_nonloot <- priogrid::prep_gold(golddata_nl)

  gold_nonloot <- priogrid::static_dummy(gold_nonloot) %>%
    dplyr::rename(goldvein_s = dummy)

  non_raster <- raster::rasterize(gold_nonloot, priogrid::prio_blank_grid(),
                                  field = "goldvein_s", fun = "first")
  return(non_raster)

}

prep_petro <- function(petro_data){
  # Load data
  petroleum <- sf::st_read(petro_data, quiet = TRUE)
  petroleum <- sf::st_set_crs(petroleum, value = priogrid::prio_crs())

  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc.year = DISC, prod.year = PROD, geometry)

  petroleum$disc.year <- priogrid::prio_NA(petroleum$disc.year, -9999)
  petroleum$prod.year <- priogrid::prio_NA(petroleum$prod.year, -9999)

  petroleum$disc.year <- priogrid::prio_earliest(petroleum$disc.year)
  petroleum$prod.year <- priogrid::prio_earliest(petroleum$prod.year)

  petroleum
}


#' Generate yearly petroleum presence dummy variable.
#'
#' @param petro_data PRIO Petroleum Dataset v. 1.2 onshore shapefile
gen_petro_y <- function(petro_data){
  petroleum <- priogrid::prep_petro(petro_data)

  petroleum <- priogrid::yearly_dummy(data = petroleum, endyear = 2003) %>%
    dplyr::rename(petroleum_y = dummy)

  petroleum <- priogrid::yearly_brick(petroleum, variable = "petroleum_y", raster.fun = "first")

}

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













