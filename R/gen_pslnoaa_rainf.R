#' @title plsnoaa_rainf_sum
#'
#' @description Generate sum cell human development index
#' for all years from 1979 through present from
#' the Global Precipitation Climatology Project (GPCP) Monthly Analysis Product.
#'
#' Link to original data: https://psl.noaa.gov/data/gridded/data.gpcp.html.
#'
#' Please cite: Adler, R.F., G.J. Huffman, A. Chang, R. Ferraro, P. Xie, J. Janowiak, B. Rudolf, U. Schneider, S. Curtis, D. Bolvin, A. Gruber, J. Susskind, and P. Arkin, 2003: The Version 2 Global Precipitation Climatology Project (GPCP) Monthly Precipitation Analysis (1979-Present). J. Hydrometeor., 4,1147-1167.
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max").
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_plsnoaa_rainf_sum <- function(input_folder, fun = "sum", interpolate_time = FALSE, interpolate_missing = FALSE){
  
  plsnoaa_rainf <- raster::brick(file.path(input_folder, "input", "pls_noaa", "gpcp", "precip.mon.mean.nc"))
  
  plsnoaa_rainf <- plsnoaa_rainf[[133:raster::nlayers(plsnoaa_rainf)]]
  
  plsnoaa_rainf <- priogrid::raster_to_pg(plsnoaa_rainf, aggregation_function = fun)
  plsnoaa_rainf <- priogrid::raster_to_tibble(plsnoaa_rainf, add_pg_index = TRUE)
  
  plsnoaa_rainf <- plsnoaa_rainf %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("X1", "X2")),
                        values_to = paste0(var),
                        names_to = "yearmonth") %>%
    dplyr::mutate(yearmonth = lubridate::ymd(stringr::str_replace(yearmonth, "X", ""))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( !!paste0(var, "_mm_month") := !!as.name(paste0(var))*as.integer(lubridate::days_in_month(yearmonth)))
  
  if (interpolate_time){
    plsnoaa_rainf <- priogrid::interpolate_pg_timeseries(plsnoaa_rainf, variable = var)
  }
  
  if (interpolate_missing){
    missing <- priogrid::missing_in_pg(plsnoaa_rainf, var, input_folder, plot_missing = FALSE)
    plsnoaa_rainf_list <- base::split(plsnoaa_rainf, plsnoaa_rainf$year)
    
    ipol_list <- parallel::mclapply(plsnoaa_rainf_list, priogrid::interpolate_crossection, variable = var, lon = "x", lat = "y", input_folder)
    
    ipol_miss <- parallel::mclapply(ipol_list, dplyr::right_join, missing, by = c("x", "y"))
    
    
    add_timevar <- function(df, time, timevar){
      df[[timevar]] <- time
      return(df)
    }
    
    ipol_tbl <- purrr::map2_dfr(ipol_miss, names(ipol_miss), add_timevar, timevar = "year") %>% dplyr::mutate(year = as.numeric(year))
    
    plsnoaa_rainf <- dplyr::bind_rows(plsnoaa_rainf, ipol_tbl)
    
    missing <- priogrid::missing_in_pg(plsnoaa_rainf, var, input_folder, plot_missing = FALSE)
    assertthat::assert_that(is.null(missing))
    
  }
  
  return(plsnoaa_rainf)
}


#' @title plsnoaa_rainf_sd
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
gen_plsnoaa_rainf_sd <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  # priogrid::gen_pop_gpw_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
  gen_plsnoaa_rainf_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_rainf_min
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
gen_plsnoaa_rainf_min <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  # priogrid::gen_pop_gpw_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
  gen_plsnoaa_rainf_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_rainf_max
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
gen_plsnoaa_rainf_max <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_rainf_sum(input_folder, fun = "max", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_rainf_mean
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
gen_plsnoaa_rainf_mean <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_rainf_sum(input_folder, fun = "mean", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_rainf_med
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
gen_plsnoaa_rainf_med <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_rainf_sum(input_folder, fun = "median", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title plsnoaa_rainf_first
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
gen_plsnoaa_rainf_first <- function(input_folder, interpolate_time = FALSE, interpolate_missing = FALSE){
  gen_plsnoaa_rainf_sum(input_folder, fun = "first", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}
