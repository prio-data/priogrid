#' @title gcp_ppp_sum
#'
#' @description Generate sum gross cell product ppp
#' for all years from 1990 through 2015 from
#' the Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015.
#'
#' Link to original data: https://doi.org/10.5061/dryad.dk1j0.
#'
#' Please cite: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0.
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max").
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 2000 to 2020. See interpolate_pg_timeseries() for details.
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @export
gen_gcp_ppp_sum <- function(input_folder, fun = "sum", interpolate_time = FALSE, interpolate_missing = FALSE){
  
  gcp <- raster::brick(file.path(input_folder, "input", "gdp_hdi", "GDP_PPP_1990_2015_5arcmin_v2.nc"))
  
  gcp <- gcp[[1:26]]
  
  gcp <- priogrid::raster_to_pg(gcp, aggregation_function = fun)
  gcp <- priogrid::raster_to_tibble(gcp, add_pg_index = TRUE)
  
  var <- paste0("gcp_ppp_", as.character(fun))
  
  gcp <- gcp %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = X1990:X2015,
                        values_to = paste0(var),
                        names_to = "year") %>%
    dplyr::mutate(year = as.numeric(stringr::str_remove(year, "X"))) %>%
    dplyr::ungroup()
  
  if (interpolate_time){
    gcp <- priogrid::interpolate_pg_timeseries(gcp, variable = var)
  }
  
  if (interpolate_missing){
    missing <- priogrid::missing_in_pg(gcp, var, input_folder, plot_missing = FALSE)
    gcp_list <- base::split(gcp, gcp$year)
    
    ipol_list <- parallel::mclapply(gcp_list, priogrid::interpolate_crossection, variable = var, lon = "x", lat = "y", input_folder)
    
    ipol_miss <- parallel::mclapply(ipol_list, dplyr::right_join, missing, by = c("x", "y"))
    
    
    add_timevar <- function(df, time, timevar){
      df[[timevar]] <- time
      return(df)
    }
    
    ipol_tbl <- purrr::map2_dfr(ipol_miss, names(ipol_miss), add_timevar, timevar = "year") %>% dplyr::mutate(year = as.numeric(year))
    
    gcp <- dplyr::bind_rows(gcp, ipol_tbl)
    
    missing <- priogrid::missing_in_pg(gcp, var, input_folder, plot_missing = FALSE)
    assertthat::assert_that(is.null(missing))
    
  }
  
  return(gcp)
}


#' @title gcp_ppp_sd
#'
#' @description Generate standard deviation of gross cell product ppp
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
gen_gcp_ppp_sd <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  # priogrid::gen_pop_gpw_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
  gen_gcp_ppp_sum(input_folder, fun = "sd", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title gcp_ppp_min
#'
#' @description Generate minimum gross cell product ppp
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
gen_gcp_ppp_min <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  # priogrid::gen_pop_gpw_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
  gen_gcp_ppp_sum(input_folder, fun = "min", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title gcp_ppp_max
#'
#' @description Generate maximum gross cell product ppp
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
gen_gcp_ppp_max <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  gen_gcp_ppp_sum(input_folder, fun = "max", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title gcp_ppp_mean
#'
#' @description Generate mean gross cell product ppp
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
gen_gcp_ppp_mean <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  gen_gcp_ppp_sum(input_folder, fun = "mean", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}


#' @title gcp_ppp_med
#'
#' @description Generate median gross cell product ppp
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
gen_gcp_ppp_med <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  gen_gcp_ppp_sum(input_folder, fun = "median", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}

#' @title gcp_ppp_first
#'
#' @description Generate first value gross cell product ppp
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
gen_gcp_ppp_first <- function(input_folder, interpolate_time = FALSE, interpolate_missing = TRUE){
  gen_gcp_ppp_sum(input_folder, fun = "first", interpolate_time = interpolate_time, interpolate_missing = interpolate_missing)
}
