#' @title pop_gpw_sum
#'
#' @description Generate sum population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4.
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Count, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4JW8BX5.
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max").
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_pop_gpw_sum <- function(input_folder, fun = "sum", interpolate_time = FALSE, interpolate_missing = FALSE){
    gpw <- raster::brick(file.path(input_folder, "pop_gpw", "data", "gpw_v4_population_count_rev11_2pt5_min.nc"))

    gpw <- gpw[[1:5]]

    gpw <- priogrid::raster_to_pg(gpw, aggregation_function = fun)
    gpw <- priogrid::raster_to_tibble(gpw, add_pg_index = TRUE)

    var <- paste0("pop_gpw_", as.character(fun))

    gpw <- gpw %>%
      dplyr::group_by(pgid) %>%
      tidyr::pivot_longer(cols = X1:X5,
                   values_to = paste0(var),
                   names_to = "year") %>%
      dplyr::mutate(year = ifelse(year == "X1", 2000,
                                  ifelse(year == "X2", 2005,
                                         ifelse(year == "X3", 2010,
                                                ifelse(year == "X4", 2015,
                                                       ifelse(year == "X5", 2020, year)))))) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::ungroup()

    if (interpolate_time){
      gpw <- priogrid::interpolate_pg_timeseries(gpw, variable = var)
    }

    if (interpolate_missing){
      missing <- priogrid::missing_in_pg(gpw, var, input_folder, plot_missing = FALSE)
      gpw_list <- base::split(gpw, gpw$year)

      ipol_list <- parallel::mclapply(gpw_list, priogrid::interpolate_crossection, variable = var, lon = "x", lat = "y", input_folder)

      ipol_miss <- parallel::mclapply(ipol_list, dplyr::right_join, missing, by = c("x", "y"))


      add_timevar <- function(df, time, timevar){
        df[[timevar]] <- time
        return(df)
      }

      ipol_tbl <- purrr::map2_dfr(ipol_miss, names(ipol_miss), add_timevar, timevar = "year") %>% dplyr::mutate(year = as.numeric(year))

      gpw <- dplyr::bind_rows(gpw, ipol_tbl)

      missing <- priogrid::missing_in_pg(gpw, var, input_folder, plot_missing = FALSE)
      assertthat::assert_that(is.null(missing))


    }

    return(gpw)
}


#' @title pop_gpw_sd
#'
#' @description Generate standard deviation of population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#' Link to original data: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4.
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Count, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4JW8BX5.
#'
#' @param input_folder path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_pop_gpw_sd <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  priogrid::gen_pop_gpw_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title pop_gpw_min
#'
#' @description Generate minimum population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4.
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Count, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4JW8BX5.
#'
#' @param input_folder path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_pop_gpw_min <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  priogrid::gen_pop_gpw_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title pop_gpw_max
#'
#' @description Generate maximum population count
#' for years 2000, 2005, 2010, 2015 and 2020 from
#' the Gridded Population of the World v.4 data.
#'
#' Link to original data: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4.
#'
#' Please cite: Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Count, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4JW8BX5.
#'
#' @param input_folder Path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_pop_gpw_max <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  gen_pop_gpw_sum(input_folder, fun = "max", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}
