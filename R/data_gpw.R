
#' @title pop_gpw_sum
#'
#' @description Generate sum population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_pop_gpw_sum <- function(input_folder, fun = "sum"){
    gpw <- raster::brick(file.path(input_folder, "pop_gpw", "data", "gpw_v4_population_count_rev11_2pt5_min.nc"))

    gpw <- gpw[[1:5]]

    # raster::extent(gpw) <- priogrid::prio_extent()

    gpw <- priogrid::raster_to_pg(gpw, aggregation_function = fun)
    gpw <- priogrid::raster_to_tibble(gpw, add_pg_index = TRUE)

    name <- paste0("pop_gpw_", as.character(fun))

    gpw <- gpw %>%
      dplyr::group_by(pgid) %>%
      tidyr::pivot_longer(cols = X1:X5,
                   values_to = paste0(name),
                   names_to = "year") %>%
      dplyr::mutate(year = ifelse(year == "X1", 2000,
                                  ifelse(year == "X2", 2005,
                                         ifelse(year == "X3", 2010,
                                                ifelse(year == "X4", 2015,
                                                       ifelse(year == "X5", 2020, year)))))) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::ungroup()

    return(gpw)
}


#' @title pop_gpw_sd
#'
#' @description Generate standard deviation population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_pop_gpw_sd <- function(input_folder){
  pop_gpw_sd <- priogrid::gen_pop_gpw_sum(input_folder, fun = "sd")

  return(pop_gpw_sd)
}


#' @title pop_gpw_min
#'
#' @description Generate minimum population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_pop_gpw_min <- function(input_folder){
  pop_gpw_min <- priogrid::gen_pop_gpw_sum(input_folder, fun = "min")

  return(pop_gpw_min)
}


#' @title pop_gpw_max
#'
#' @description Generate maximum population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_pop_gpw_max <- function(input_folder){
  pop_gpw_max <- priogrid::gen_pop_gpw_sum(input_folder, fun = "max")

  return(pop_gpw_max)
}



