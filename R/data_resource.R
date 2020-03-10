# Resource data

prep_resource <- function(data, endyear = NULL, static = TRUE){
  if(static == TRUE){
    data <- data %>%
      dplyr::filter(is.na(disc.year) & is.na(prod.year))
    return(data)
  }
  else {
  data <- data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(year = purrr::map2(min(disc.year, prod.year, na.rm = TRUE), endyear, `:`)) %>%
    tidyr::unnest(year) %>%
    dplyr::ungroup()
  return(data)
  }
}


# Diamond data

prep_diamonds <- function(input_folder){

  diamonds <- sf::st_read(file.path(input_folder, "DIADATA.shp"), quiet = TRUE)
  diamonds <- sf::st_transform(diamonds, crs = priogrid::prio_crs())

  diamonds$disc.year <- lubridate::year(diamonds$DISC)
  diamonds$PROD <- base::as.Date(diamonds$PROD, format = "%d/%m/%Y")
  diamonds$prod.year <- lubridate::year(diamonds$PROD)

  diamonds <- diamonds %>%
    dplyr::select(id = PRIMKEY, disc.year, prod.year, DIAINFO, geometry) %>%
    dplyr::mutate(diamprim = ifelse(DIAINFO == "P", yes = 1, no = NA),
                  diamsec = ifelse(DIAINFO == "S", yes = 1, no = NA)) %>%
    dplyr::filter(diamprim == 1 | diamsec == 1)

  return(diamonds)
}


#' Generate yearly primary diamond presence dummy.
#'
#' @param diamond_data DIADATA shapefile from the PRIO Diamond Resources dataset.
gen_diamprim_y <- function(input_folder, variable = "diamprim_y"){

  diamonds <- prep_diamonds(input_folder) %>%
  dplyr::rename(diamprim_y = diamprim, diamsec_y = diamsec) %>%
  prep_resource(endyear = 2005, static = FALSE)

  diamonds <- priogrid::panel_to_pg(diamonds, timevar = "year", variable = variable,
                               need_aggregation = TRUE, fun = "first")

  return(diamonds)

}

#' Generate yearly secondary diamond presence dummy.
gen_diamsec_y <- function(input_folder){
  diamonds <- gen_diamprim_y(input_folder, variable = "diamsec_y")

  return(diamonds)
}

#' Generate static diamond presence dummy
#' including records without known start year.

gen_diamprim_s <- function(input_folder, variable = "diamprim_s"){
  diamonds <- prep_diamonds(input_folder) %>%
    dplyr::rename(diamsec_s = diamsec, diamprim_s = diamprim)

  diamonds <- priogrid::vector_to_pg(diamonds, variable = variable, need_aggregation = TRUE, fun = "first")

  diamonds <- priogrid::raster_to_tibble(diamonds, add_pg_index = TRUE)

  return(diamonds)
}

gen_diamsec_s <- function(input_folder){
  gen_diamonds_s(input_folder, "diamsec")
}


# Drug data

#' Generate yearly dummy identifying ongoing large-scale drug cultivation
#' (coca bush, opium poppy and/or cannabis).
#'

gen_drug_y <- function(input_folder){
  cannabis <- sf::st_read(file.path(input_folder, "CANNABIS.shp"), stringsAsFactors = FALSE, quiet = TRUE)
  coca <- sf::st_read(file.path(input_folder, "COCA BUSH.shp"), stringsAsFactors = FALSE, quiet = TRUE)
  opium <- sf::st_read(file.path(input_folder, "OPIUM POPPY.shp"), stringsAsFactors = FALSE, quiet = TRUE)

  cleanup <- function(data, vars){
    data <- data %>%
      janitor::clean_names() %>%
      dplyr::select(tidyselect::all_of(vars))
  }
  vars <- c("id", "country", "begin", "end", "geometry")

  drugs <- rbind(cleanup(cannabis, vars),
                 cleanup(coca, vars),
                 cleanup(opium, vars))

  drugs$end[which(is.na(drugs$end))] <- 2002
  drugs$begin[drugs$begin < 1946] <- 1946

  drugs <- drugs %>%
    sf::st_set_crs(value = priogrid::prio_crs())
    dplyr::group_by(id) %>%
    dplyr::mutate(year = purrr::map2(begin, end, `:`),
                  drug_y = 1) %>%
    tidyr::unnest(year) %>%
    dplyr::ungroup() %>%
    sf::st_cast("MULTIPOLYGON")

  drugs <- priogrid::panel_to_pg(drugs, timevar = "year", variable = "drug_y",
                               need_aggregation = TRUE, fun = "first")

  return(drugs)

}


# Gem data

prep_gems <- function(input_folder){
  gems <- sf::st_read(file.path(input_folder, "GEMDATA.shp"), quiet = TRUE)

  gems <- gems %>%
    dplyr::select(id = PRIMKEY, disc.year = DISC_Y, prod.year = PRO_Y, geometry)

  gems$disc.year[gems$disc.year == 0] <- NA
  gems$prod.year[gems$prod.year == 0] <- NA

  gems$disc.year[gems$disc.year < 1946] <- 1946
  gems$prod.year[gems$prod.year < 1946] <- 1946

  return(gems)
}


gen_gem_y <- function(input_folder){
  gems <- prep_gems(input_folder)

  gems <- prep_resource(gems, endyear = 2004, static = FALSE) %>%
    dplyr::mutate(gem_y = 1)

  gems <- priogrid::panel_to_pg(gems, timevar = "year", variable = "gem_y",
                                 need_aggregation = TRUE, fun = "first")

  return(gems)

}



#' Generate static gem dummy.
#'
#' Function to generate static gem presence dummy for records without known discovery or start of production year.

gen_gem_s <- function(input_folder){
  gems <- prep_gems(input_folder)

  gems <- prep_resource(gems, static = TRUE) %>%
    dplyr::mutate(gem_s = 1)

  gems <- priogrid::vector_to_pg(gems, variable = "gem_s", fun = "first", need_aggregation = TRUE)

  gems <- priogrid::raster_to_tibble(gems, add_pg_index = TRUE)

  return(gems)

}


# Gold data
input_folder <- "~/Dropbox/formatted_raw/gold/data/"
prep_gold <- function(input_folder, data_file = "dGOLD_L.shp"){
  gold <- sf::st_read(file.path(input_folder, data_file), quiet = TRUE)
  gold <- sf::st_set_crs(gold, value = priogrid::prio_crs())

  gold <- gold %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry)

  gold$disc.year[gold$disc.year == 9999] <- NA
  gold$prod.year[gold$prod.year == 9999] <- NA

  return(gold)

}

#' Generate yearly dummy identifying lootable placer gold deposits.

gen_goldplacer_y <- function(input_folder){
  gold <- prep_gold(input_folder, data_file = "dGOLD_L.shp")

  gold <- prep_resource(gold, endyear = 2012, static = FALSE) %>%
    dplyr::mutate(goldplacer_y = 1)

  gold <- priogrid::panel_to_pg(gold, timevar = "year", variable = "goldplacer_y", need_aggregation = TRUE,
                                 fun = "first")

  return(gold)
}

#' Generate yearly dummy identifying semi-lootable surface gold deposits.
gen_goldsurface_y <- function(input_folder){
  gold <- prep_gold(input_folder, data_file = "dGOLD_S.shp")

  gold <- prep_resource(gold_semiloot, endyear = 2012, static = FALSE) %>%
    dplyr::mutate(goldsurface_y = 1)

  gold <- priogrid::panel_to_pg(gold, timevar = "year", variable = "goldsurface_y",
                                need_aggregation = TRUE, fun = "first")

  return(gold)

}


#' Generate yearly dummy identifying non-lootable vein gold deposits.
gen_goldvein_y <- function(input_folder){
  gold <- prep_gold(input_folder, data_file = "dGOLD_NL.shp")

  gold <- prep_resource(gold, endyear = 2012, static = FALSE) %>%
    dplyr::mutate(goldvein_y = 1)

  gold <- priogrid::panel_to_pg(gold, timevar = "year", variable = "goldvein_y",
                             need_aggregation = TRUE, fun = "first")

  return(gold)

}

# Static gold

gen_goldplacer_s <- function(input_folder){

  gold <- prep_gold(input_folder, data_file = "dGOLD_L.shp")

  gold <- prep_resource(gold, static = TRUE) %>%
    dplyr::mutate(goldplacer_s = 1)

  gold <- priogrid::vector_to_pg(gold, variable = "goldplacer_s", fun = "first", need_aggregation = TRUE)

  gold <- priogrid::raster_to_tibble(gold, add_pg_index = TRUE)

  return(gold)

}

gen_goldsurface_s <- function(input_folder){

  gold <- prep_gold(input_folder, data_file = "dGOLD_S.shp")

  gold <- prep_resource(gold, static = TRUE) %>%
    dplyr::mutate(goldsurface_s = 1)

  gold <- priogrid::vector_to_pg(gold, variable = "goldsurface_s", fun = "first", need_aggregation = TRUE)

  gold <- priogrid::raster_to_tibble(gold, add_pg_index = TRUE)

  return(gold)

}



gen_goldvein_s <- function(input_folder){

  gold <- prep_gold(input_folder, data_file = "dGOLD_NL.shp")

  gold <- prep_resource(gold, static = TRUE) %>%
    dplyr::mutate(goldvein_s = 1)

  gold <- priogrid::vector_to_pg(gold, variable = "goldvein_s", fun = "first", need_aggregation = TRUE)

  gold <- priogrid::raster_to_tibble(gold, add_pg_index = TRUE)

  return(gold)

}


# Petroleum data

prep_petro <- function(input_folder){
  petroleum <- sf::st_read(file.path(input_folder, "Petrodata_Onshore_V1.2.shp"), quiet = TRUE)

  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc.year = DISC, prod.year = PROD, geometry)

  petroleum$disc.year[petroleum$disc.year == -9999] <- NA
  petroleum$prod.year[petroleum$prod.year == -9999] <- NA

  petroleum$disc.year[petroleum$disc.year < 1946] <- 1946
  petroleum$prod.year[petroleum$prod.year < 1946] <- 1946

  return(petroleum)
}


#' Generate yearly petroleum presence dummy variable.

gen_petro_y <- function(input_folder){
  petroleum <- prep_petro(input_folder)

  petroleum <- prep_resource(petroleum, endyear = 2003, static = FALSE) %>%
    dplyr::mutate(petroleum_y = 1)

  petroleum <- priogrid::panel_to_pg(petroleum, timevar = "year", variable = "petroleum_y", fun = "first",
                                     need_aggregation = TRUE)

}

#' Generate static petroleum presence dummy variable
#' for records without known year of discovery or production start.

gen_petro_s <- function(input_folder){
  petroleum <- prep_petro(input_folder)

  petroleum <- prep_resource(petroleum, static = TRUE) %>%
    dplyr::mutate(petroleum_s = 1)

  petroleum <- priogrid::vector_to_pg(petroleum, variable = "petroleum_s", fun = "first",
                                             need_aggregation = TRUE)

  petroleum <- priogrid::raster_to_tibble(petroleum, add_pg_index = TRUE)

  return(petroleum)

}












