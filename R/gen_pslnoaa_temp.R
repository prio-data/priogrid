#' @title plsnoaa_temp_sum
#'
#' @description Generate mean temperature (2m above land)
#' (also has longtermmean but format is unclear, annual?)
#' for all years from 1990 through today from
#' the GHCN_CAMS Gridded 2m Temperature (Land) data.
#'
#' Link to original data: https://psl.noaa.gov/data/gridded/data.ghcncams.html.
#'
#' Please cite: Fan, Y., and H. van den Dool (2008), A global monthly land surface air temperature analysis for 1948-present, J. Geophys. Res., 113, D01103, doi:10.1029/2007JD008470..
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max").
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_sum <- function(input_folder, fun = "sum", interpolate_time = FALSE, interpolate_missing = FALSE){
  
  plsnoaa_temp <- raster::brick(file.path(input_folder, "input", "pls_noaa", "ghcn_cams", "air.mon.mean.nc"))
  
  ## [[1]] is January 1948, we start from January 1990 which corresponds to [[505]]
  plsnoaa_temp <- plsnoaa_temp[[505:raster::nlayers(plsnoaa_temp)]]
  
  plsnoaa_temp <- priogrid::raster_to_pg(plsnoaa_temp, aggregation_function = fun)
  plsnoaa_temp <- priogrid::raster_to_tibble(plsnoaa_temp, add_pg_index = TRUE)
  
  if (fun != "sd") {
    var <- paste0("plsnoaa_temp_", as.character(fun), "_K")
  } else {
    var <- paste0("plsnoaa_temp_", as.character(fun))
  }
  
  plsnoaa_temp <- plsnoaa_temp %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("X1", "X2")),
                        values_to = paste0(var),
                        names_to = "yearmonth") %>%
    dplyr::mutate(yearmonth = lubridate::ymd(stringr::str_replace(yearmonth, "X", ""))) %>%
    dplyr::ungroup()
  
  ## convert from Kelvin to Celsius
  if (fun != "sd") {
    varC <- paste0("plsnoaa_temp_", as.character(fun), "_C")
    
    plsnoaa_temp <- plsnoaa_temp %>%
      dplyr::mutate( !!paste0(varC) := !!as.name(paste0(var))-273.15)
  }
  
  
  if (interpolate_time){
    plsnoaa_temp <- priogrid::interpolate_pg_timeseries(plsnoaa_temp, variable = var)
  }
  
  if (interpolate_missing){
    missing <- priogrid::missing_in_pg(plsnoaa_temp, var, input_folder, plot_missing = FALSE)
    plsnoaa_temp_list <- base::split(plsnoaa_temp, plsnoaa_temp$year)
    
    ipol_list <- parallel::mclapply(plsnoaa_temp_list, priogrid::interpolate_crossection, variable = var, lon = "x", lat = "y", input_folder)
    
    ipol_miss <- parallel::mclapply(ipol_list, dplyr::right_join, missing, by = c("x", "y"))
    
    
    add_timevar <- function(df, time, timevar){
      df[[timevar]] <- time
      return(df)
    }
    
    ipol_tbl <- purrr::map2_dfr(ipol_miss, names(ipol_miss), add_timevar, timevar = "year") %>% dplyr::mutate(year = as.numeric(year))
    
    plsnoaa_temp <- dplyr::bind_rows(plsnoaa_temp, ipol_tbl)
    
    missing <- priogrid::missing_in_pg(plsnoaa_temp, var, input_folder, plot_missing = FALSE)
    assertthat::assert_that(is.null(missing))
    
    
  }
  
  return(plsnoaa_temp)
}


#' @title plsnoaa_temp_sd
#'
#' @description Generate standard deviation of cell human development index
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_sd <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  # priogrid::gen_pop_gpw_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
  gen_plsnoaa_temp_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_temp_min
#'
#' @description Generate minimum cell human development index
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_min <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  # priogrid::gen_pop_gpw_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
  gen_plsnoaa_temp_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_temp_max
#'
#' @description Generate maximum cell human development index
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder Path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_max <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_temp_sum(input_folder, fun = "max", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_temp_mean
#'
#' @description Generate mean cell human development index
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder Path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_mean <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_temp_sum(input_folder, fun = "mean", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_temp_med
#'
#' @description Generate median cell human development index
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder Path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_med <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_temp_sum(input_folder, fun = "median", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_temp_first
#'
#' @description Generate first value of cell human development index
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder Path to [pg-folder].
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_temp_first <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_temp_sum(input_folder, fun = "first", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}
