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

  diamonds <- sf::st_read(file.path(input_folder, "resource",
                                    "data", "DIADATA.shp"), quiet = TRUE)
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



#' @title diamprim_y
#'
#' @description Generate dummy variable for whether
#' primary (kimberlite or lamproite) diamond deposits have
#' been found within the given grid cell for any given year,
#' based on the Diamond Resources dataset v1a.
#' This variable only codes deposits that have a known year of
#' discovery or start of production. For a complete picture,
#' these data must therefore be combined with the diamprim_s data.
#' Available only until 2005.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_diamprim_y <- function(input_folder, variable = "diamprim_y"){

  diamonds <- priogrid::prep_diamonds(input_folder) %>%
  dplyr::rename(diamprim_y = diamprim, diamsec_y = diamsec) %>%
  priogrid::prep_resource(endyear = 2005, static = FALSE)

  diamonds <- priogrid::panel_to_pg(diamonds, timevar = "year", variable = variable,
                               need_aggregation = TRUE, fun = "first")

  return(diamonds)

}

#' @title diamsec_y
#'
#' @description Generate dummy variable for whether
#' secondary (alluvial) diamond deposits have been found
#' within the given grid cell for any given year, based on
#' the Diamond Resources dataset v1a. This variable only codes
#' those deposits that have a known year of discovery or start
#' of production. For a complete picture, these data must
#' therefore be combined with the diamsec_s data.
#' Available only until 2005.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_diamsec_y <- function(input_folder){
  diamonds <- priogrid::gen_diamprim_y(input_folder, variable = "diamsec_y")

  return(diamonds)
}


#' @title diamprim_s
#'
#' @description Generate dummy variable for whether
#' primary (kimberlite or lamproite) diamond deposits have
#' been found within the given grid cell, based on the
#' Diamond Resources dataset v1a. This variable only codes
#' those deposits that do now have a known year of discovery
#' or start of production. For a complete picture, these data
#' must therefore be combined with the diamprim_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_diamprim_s <- function(input_folder, variable = "diamprim_s"){
  diamonds <- priogrid::prep_diamonds(input_folder) %>%
    dplyr::rename(diamsec_s = diamsec, diamprim_s = diamprim)

  diamonds <- priogrid::vector_to_pg(diamonds, variable = variable, need_aggregation = TRUE, fun = "first")

  diamonds <- priogrid::raster_to_tibble(diamonds, add_pg_index = TRUE)

  return(diamonds)
}

#' @title Generate diamsec_s
#'
#' @description Generate dummy variable for whether
#' secondary (alluvial) diamond deposits have been found within
#' the given grid cell, based on the Diamond Resources dataset v1a.
#' This variable only codes those deposits that do not have a known
#' year of discovery or start of production. For a complete picture,
#' these data must therefore be combined with the diamsec_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_diamsec_s <- function(input_folder){
  priogrid::gen_diamonds_s(input_folder, "diamsec")
}


# Drug data

#' @title drug_y
#'
#' @description Generate dummy variable for whether large-scale
#' drug cultivation (coca bush, opium poppy, or cannabis) is ongoing
#' within the given grid cell, based on the DRUGDATA dataset.
#' Available only until 2002.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_drug_y <- function(input_folder){
  cannabis <- sf::st_read(file.path(input_folder, "resource", "data",
                                    "CANNABIS.shp"), stringsAsFactors = FALSE, quiet = TRUE)
  coca <- sf::st_read(file.path(input_folder, "resource", "data",
                                "COCA BUSH.shp"), stringsAsFactors = FALSE, quiet = TRUE)
  opium <- sf::st_read(file.path(input_folder, "resource", "data",
                                 "OPIUM POPPY.shp"), stringsAsFactors = FALSE, quiet = TRUE)

  cleanup <- function(data, vars){
    data <- data %>%
      janitor::clean_names() %>%
      dplyr::select(tidyselect::all_of(vars))
  }
  vars <- c("id", "country", "begin", "end", "geometry")

  drugs <- rbind(cleanup(cannabis, vars),
                 cleanup(coca, vars),
                 cleanup(opium, vars))

  drugs <- drugs %>%
    dplyr::mutate(end = tidyr::replace_na(end, 2002),
           begin = ifelse(begin < 1946, 1946, begin)) %>%
    sf::st_set_crs(value = priogrid::prio_crs()) %>%
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
  gems <- sf::st_read(file.path(input_folder, "resource", "data",
                                "GEMDATA.shp"), quiet = TRUE)

  gems <- gems %>%
    dplyr::select(id = PRIMKEY, disc.year = DISC_Y, prod.year = PRO_Y, geometry) %>%
    dplyr::mutate(disc.year = dplyr::na_if(disc.year, 0),
                  prod.year = dplyr::na_if(prod.year, 0),
                  disc.year = ifelse(disc.year < 1946, 1946, disc.year),
                  prod.year = ifelse(prod.year < 1946, 1946, prod.year))

  return(gems)
}


#' @title gem_y
#'
#' @description Generate dummy variable for whether
#' gem deposits have been found within the given grid cell,
#' based on the GEMDATA dataset. This variable only codes
#' those deposits that have a known year of discovery or
#' start of production. For a complete picture, these data
#' must therefore be combined with the gem_s data.
#' Available only until 2004.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_gem_y <- function(input_folder){
  gems <- priogrid::prep_gems(input_folder)

  gems <- priogrid::prep_resource(gems, endyear = 2004, static = FALSE) %>%
    dplyr::mutate(gem_y = 1)

  gems <- priogrid::panel_to_pg(gems, timevar = "year", variable = "gem_y",
                                 need_aggregation = TRUE, fun = "first")

  return(gems)

}


#' @title gem_s
#'
#' @description Generate dummy variable for whether
#' gem deposits have been found within the given grid cell,
#' based on the GEMDATA dataset. This variable only codes
#' those deposits that do not have a known year of discovery
#' or start of production. For a complete picture, these data
#' must therefore be combined with the gem_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_gem_s <- function(input_folder){
  gems <- priogrid::prep_gems(input_folder)

  gems <- priogrid::prep_resource(gems, static = TRUE) %>%
    dplyr::mutate(gem_s = 1)

  gems <- priogrid::vector_to_pg(gems, variable = "gem_s", fun = "first", need_aggregation = TRUE)

  gems <- priogrid::raster_to_tibble(gems, add_pg_index = TRUE)

  return(gems)

}


# Gold data

prep_gold <- function(input_folder, data_file = "dGOLD_L.shp"){
  gold <- sf::st_read(file.path(input_folder, "resource",
                                "data", data_file), quiet = TRUE)
  gold <- sf::st_set_crs(gold, value = priogrid::prio_crs())

  gold <- gold %>%
    dplyr::select(id = PRIMKEY, gwno = COWcode, prod.year = PRODyear, disc.year = DISCyear, geometry) %>%
    dplyr::mutate(disc.year = dplyr::na_if(disc.year, 9999),
                  prod.year = dplyr::na_if(prod.year, 9999))

  return(gold)

}

#' @title goldplacer_y
#'
#' @description Generate dummy variable for whether
#' placer gold deposits have been found within the given
#' grid cell, based om the dGOLD_L subset of the
#' GOLDATA dataset v1.2. Available only until 2012.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_goldplacer_y <- function(input_folder){
  gold <- priogrid::prep_gold(input_folder, data_file = "dGOLD_L.shp")

  gold <- priogrid::prep_resource(gold, endyear = 2012, static = FALSE) %>%
    dplyr::mutate(goldplacer_y = 1)

  gold <- priogrid::panel_to_pg(gold, timevar = "year", variable = "goldplacer_y", need_aggregation = TRUE,
                                 fun = "first")

  return(gold)
}


#' @title goldsurface_y
#'
#' @description Generate dummy variable for whether
#' surface gold deposits, defined as deposits that are
#' located near the surfaec but "do not hold enough information
#' to be properly defined as lootable" have been found within
#' the given grid cell, based on the dGOLD_S subset of the
#' GOLDATA dataset v1.2. This variable only codes the deposits
#' that have a known year of discovery or start of production.
#' For a complete picture, these data must therefore be combined
#' with the goldsurface_s data. Available only until 2012.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_goldsurface_y <- function(input_folder){
  gold <- priogrid::prep_gold(input_folder, data_file = "dGOLD_S.shp")

  gold <- priogrid::prep_resource(gold, endyear = 2012, static = FALSE) %>%
    dplyr::mutate(goldsurface_y = 1)

  gold <- priogrid::panel_to_pg(gold, timevar = "year", variable = "goldsurface_y",
                                need_aggregation = TRUE, fun = "first")

  return(gold)

}


#' @title goldvein_y
#'
#' @description Generate dummy variable for whether
#' vein gold deposits have been found within the given grid cell,
#' based on the dGOLD_NL subset of the GOLDATA dataset v1.2.
#' This variable only codes those deposits that have a known
#' year of discovery or start of production. For a complete picture
#' these data must therefore be combined with the goldvein_s data.
#' Available only until 2012.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_goldvein_y <- function(input_folder){
  gold <- priogrid::prep_gold(input_folder, data_file = "dGOLD_NL.shp")

  gold <- priogrid::prep_resource(gold, endyear = 2012, static = FALSE) %>%
    dplyr::mutate(goldvein_y = 1)

  gold <- priogrid::panel_to_pg(gold, timevar = "year", variable = "goldvein_y",
                             need_aggregation = TRUE, fun = "first")

  return(gold)

}


#' @title goldplacer_s
#'
#' @description Generate dummt variable for whether
#' placer gold deposits have been found within the given grid cell,
#' based on the dGOLD_L subset of the GOLDATA v1.2 dataset.
#' This variable only codes those deposits that do not have a known
#' year of discovery or start of production. For a complete picture,
#' these data must therefore be combined with the goldplacer_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_goldplacer_s <- function(input_folder){

  gold <- priogrid::prep_gold(input_folder, data_file = "dGOLD_L.shp")

  gold <- priogrid::prep_resource(gold, static = TRUE) %>%
    dplyr::mutate(goldplacer_s = 1)

  gold <- priogrid::vector_to_pg(gold, variable = "goldplacer_s", fun = "first", need_aggregation = TRUE)

  gold <- priogrid::raster_to_tibble(gold, add_pg_index = TRUE)

  return(gold)

}

#' @title goldsurface_s
#'
#' @description Generate dummy variable for whether
#' surface gold deposits have been found within the given grid cell,
#' based on the dGOLD_S subset of the GOLDATA v1.2 dataset.
#' This variable only codes those deposits that do not have a known
#' year of discovery or start of production. For a complete picture,
#' these data must therefore be combined with the goldsurface_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_goldsurface_s <- function(input_folder){

  gold <- priogrid::prep_gold(input_folder, data_file = "dGOLD_S.shp")

  gold <- priogrid::prep_resource(gold, static = TRUE) %>%
    dplyr::mutate(goldsurface_s = 1)

  gold <- priogrid::vector_to_pg(gold, variable = "goldsurface_s", fun = "first", need_aggregation = TRUE)

  gold <- priogrid::raster_to_tibble(gold, add_pg_index = TRUE)

  return(gold)

}


#' @title goldvein_s
#'
#' @description Generate dummy variable for whether
#' vein gold deposits have been found within the given grid cell,
#' based on the dGOLD_NL subset of the GOLDATA v1.2 dataset.
#' This variable only codes those deposits that do not have a known
#' year of discovery or start of production. For a complete picture,
#' these data must therefore be combined with the goldvein_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_goldvein_s <- function(input_folder){

  gold <- priogrid::prep_gold(input_folder, data_file = "dGOLD_NL.shp")

  gold <- priogrid::prep_resource(gold, static = TRUE) %>%
    dplyr::mutate(goldvein_s = 1)

  gold <- priogrid::vector_to_pg(gold, variable = "goldvein_s", fun = "first", need_aggregation = TRUE)

  gold <- priogrid::raster_to_tibble(gold, add_pg_index = TRUE)

  return(gold)

}


# Petroleum data

prep_petro <- function(input_folder){
  petroleum <- sf::st_read(file.path(input_folder, "resource", "data",
                                     "Petrodata_Onshore_V1.2.shp"), quiet = TRUE)

  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc.year = DISC, prod.year = PROD, geometry) %>%
    dplyr::mutate(disc.year = dplyr::na_if(disc.year, -9999),
                  prod.year = dplyr::na_if(prod.year, -9999),
                  disc.year = ifelse(disc.year < 1946, 1946, disc.year),
                  prod.year = ifelse(prod.year < 1946, 1946, prod.year))

  return(petroleum)
}


#' @title petroleum_y
#'
#' @description Generate dummy variable for whether
#' onshore petroleum deposits have been found within
#' the given grid cell for any given year, based on the
#' Petroleum Dataset v1.2. This variable only codes those
#' petroleum deposits that have a known year of discovery or
#' start of production. For a complete picture, these data
#' must therefore be combined with the petroleum_s data.
#' Available only until 2003.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_petroleum_y <- function(input_folder){
  petroleum <- priogrid::prep_petro(input_folder)

  petroleum <- priogrid::prep_resource(petroleum, endyear = 2003, static = FALSE) %>%
    dplyr::mutate(petroleum_y = 1)

  petroleum <- priogrid::panel_to_pg(petroleum, timevar = "year", variable = "petroleum_y", fun = "first",
                                     need_aggregation = TRUE)

  return(petroleum)

}


#' @title petroleum_s
#'
#' @description Generate dummy variable for whether
#' onshore petroleum deposits have been found within
#' the given grid cell, based on the Petroleum Dataset v1.2.
#' This variable only codes those petroleum deposits that do not
#' have a known year of discovery or start of production.
#' For a complete picture, these data must therefore be
#' combined with the petroleum_y data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_petroleum_s <- function(input_folder){
  petroleum <- priogrid::prep_petro(input_folder)

  petroleum <- priogrid::prep_resource(petroleum, static = TRUE) %>%
    dplyr::mutate(petroleum_s = 1)

  petroleum <- priogrid::vector_to_pg(petroleum, variable = "petroleum_s", fun = "first",
                                             need_aggregation = TRUE)

  petroleum <- priogrid::raster_to_tibble(petroleum, add_pg_index = TRUE)

  return(petroleum)

}
