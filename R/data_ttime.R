#' @title ttime_avg
#'
#' @description Generate average travel time to
#' nearest urban center within each cell.
#' Snapshot for 2014 only. Drawn from the
#' Malaria Atlas Project's Accessibility to Cities data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export


gen_ttime_avg <- function(input_folder){
  ttime <- raster::ratster(file.path(input_folder, "ttime", "data", "2015_accessibility_to_cities_v1.0.tif"))

  raster::extent(ttime) <- priogrid::prio_extent()

  ttime1 <- priogrid::rasterextent_to_pg(ttime)

  ttime1 <- priogrid::raster_to_tibble(ttime1, add_pg_index = TRUE)

  names(ttime1)[3] <- "ttime_avg"

  return(ttime1)

}

