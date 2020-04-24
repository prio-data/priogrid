#' @title ttime_mean
#'
#' @description Generate average travel time to
#' nearest urban center within each cell.
#' Snapshot for 2014 only. Drawn from the
#' Malaria Atlas Project's Accessibility to Cities data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export


gen_ttime_mean <- function(input_folder){
  ttime_mean <- priogrid::gen_ttime(input_folder, fun = "mean")

  names(ttime_mean)[3] <- "ttime_avg"

  return(ttime_mean)
}


#' @title ttime_sd
#'
#' @description Generate standard deviation of travel time to
#' nearest urban center within each cell.
#' Snapshot for 2014 only. Drawn from the
#' Malaria Atlas Project's Accessibility to Cities data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_ttime_sd <- function(input_folder){
  ttime_sd <- priogrid::gen_ttime(input_folder, fun = "sd")

  names(ttime_sd)[3] <- "ttime_sd"

  return(ttime_sd)

}


#' @title ttime_min
#'
#' @description Generate minimum travel time to
#' nearest urban center within each cell.
#' Snapshot for 2014 only. Drawn from the
#' Malaria Atlas Project's Accessibility to Cities data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_ttime_min <- function(input_folder){
  ttime_min <- priogrid::gen_ttime(input_folder, fun = "min")

  names(ttime_min)[3] <- "ttime_min"

  return(ttime_min)
}

#' @title ttime_max
#'
#' @description Generate maximum travel time to
#' nearest urban center within each cell.
#' Snapshot for 2014 only. Drawn from the
#' Malaria Atlas Project's Accessibility to Cities data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_ttime_max <- function(input_folder){
  ttime_max <- priogrid::gen_ttime(input_folder, fun = "max")

  names(ttime_max)[3] <- "ttime_max"
}


# Preparation function

gen_ttime <- function(input_folder, fun){
  ttime <- raster::raster(file.path(input_folder, "ttime", "data", "2015_accessibility_to_cities_v1.0.tif"))

  raster::extent(ttime) <- priogrid::prio_extent()

  ttime <- priogrid::raster_to_pg(ttime, aggregation_function = fun)

  ttime <- priogrid::rasterextent_to_pg(ttime)

  ttime <- priogrid::raster_to_tibble(ttime, add_pg_index = TRUE)

  return(ttime)

}

