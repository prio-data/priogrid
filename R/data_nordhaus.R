#' gcp_mer
#'
#' Indicates the gross cell product, measured in USD, based on the G-Econ dataset v. 4.0.
#' The original G-Econ data represent the total economic activity at a 1x1 degree resolution, so when assigning this to PRIO-GRID
#' we distribute the total value across the number of contained PRIO-GRID land cells. In border areas, the G-Econ 1x1 degree cells
#' might overlap with PRIO-GRID cells allocated to a neighboring country. To minimize bias, PRIO-GRID only extracts G-Econ data for
#' cells that have the same country code as the G-Econ cell represents. This variable is only available for five-year intervals between 1990 and 2005,
#' but function allows for interpolation of yearly values from 1990 to 2005.
#'
#' Link to original data: https://gecon.yale.edu/.
#'
#' Please cite: Nordhaus, William D. (2006) Geography and macroeconomics: New data and new findings. Proceedings of the National Academy of Sciences of the USA, 103(10): 3510-3517.
#'
#' @param gecon_data path to [pg-folder].
#' @param variable one of c("GCP_MER", "GCP_PPP")
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 1990 to 2005 See interpolate_pg_timeseries() for details.
#'
#' @export
gen_gcp_mer <- function(input_folder, variable = "GCP_MER", interpolate_time = FALSE){
  gcp <- readxl::read_xls(file.path(input_folder, "gecon", "data", "Gecon40_post_final.xls"), sheet = 1, progress = FALSE)

  suffix <- sub("GCP_", "", variable)

  gcp <- gcp %>%
    dplyr::select(LAT, LONGITUDE, AREA, MER1990_40, MER1995_40, MER2000_40, MER2005_40, PPP1990_40, PPP1995_40, PPP2000_40, PPP2005_40) %>%
    dplyr::filter(AREA > 0) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with(suffix)) %>%
    dplyr::group_by(LAT, LONGITUDE, name) %>%
    dplyr::mutate(area_share = AREA / sum(AREA, na.rm = T)) %>%
    dplyr::mutate(value = log(value+1)*area_share) %>%
    dplyr::summarise(value = exp(sum(value, na.rm = T))-1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(grepl(suffix, name))

  gcp$year <- as.numeric(sub("_40", "", sub(suffix, "", gcp$name)))
  time_fact <- factor(gcp$year)

  gcp_list <- dplyr::select(gcp, LONGITUDE, LAT, value)
  gcp_list <- base::split(gcp_list, time_fact, sep = "_")
  rast_list <- parallel::mclapply(gcp_list, raster::rasterFromXYZ, crs = priogrid::prio_crs())
  rast_list <- parallel::mclapply(rast_list, priogrid::raster_to_pg)
  pg_tibble <- parallel::mclapply(rast_list, priogrid::raster_to_tibble, add_pg_index = TRUE)

  add_timevar <- function(df, time, timevar){
    df[[timevar]] <- time
    return(df)
  }

  pg_tibble <- purrr::map2_dfr(pg_tibble, names(pg_tibble), add_timevar, timevar = "year")

  names(pg_tibble) <- c("x", "y", tolower(variable), "pgid", "year")
  pg_tibble$year <- as.numeric(pg_tibble$year)

  if(interpolate_time){
    pg_tibble <- priogrid::interpolate_pg_timeseries(pg_tibble, variable = tolower(variable))
  }

  return(pg_tibble)
}


#' gcp_ppp
#'
#' Indicates the gross cell product, measured in USD using purchasing-power-parity, based on the G-Econ dataset v4.0, last modified May 2011.
#' Else similar to gcp_mer, but uses USD at purchasing-power-parity which corrects for each currency's purchasing power.
#' This variable is only available for 1990, 1995, 2000, and 2005, but function allows for temporal interpolation that gives yearly values from 1990 to 2005.
#'
#' @param input_folder path to [pg-folder]
#' @param interpolate_time if `TRUE`, data is interpolated to all years from 1990 to 2005. See interpolate_pg_timeseries() for details.
#'
#' Link to original data: https://gecon.yale.edu/.
#'
#' Please cite: Nordhaus, William D. (2006) Geography and macroeconomics: New data and new findings. Proceedings of the National Academy of Sciences of the USA, 103(10): 3510-3517.
#'
#' @export
gen_gcp_ppp <- function(input_folder, interpolate_time = FALSE){
  gen_gcp_mer(input_folder, variable = "GCP_PPP", interpolate_time = interpolate_time)
}


#' Generate gcp_qual variable
#'
#' Indicates the quality of the GCP values, based on the G-Econ dataset v4.0, last modified May 2011.
#' Quality is a measure of the quality of the economic data. Quality = 1 for countries for which the data are consistent,
#' but it does not capture the quality of the underlying country statistics. In general, quality < 1 indicates that there
#' are major inconsistencies in one of the underlying data inputs into GCP. See the G-Econ definition table, available at http://gecon.yale.edu/.
#'
#' Link to original data: https://gecon.yale.edu/.
#'
#' Please cite: Nordhaus, William D. (2006) Geography and macroeconomics: New data and new findings. Proceedings of the National Academy of Sciences of the USA, 103(10): 3510-3517.
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_gcp_qual <- function(input_folder){
  gcp <- readxl::read_xls(file.path(input_folder, "gecon", "data", "Gecon40_post_final.xls"), sheet = 1, progress = FALSE)

  gcp <- gcp %>%
    dplyr::select(LAT, LONGITUDE, QUALITY) %>%
    dplyr::group_by(LAT, LONGITUDE) %>%
    dplyr::summarise(quality = mean(QUALITY, na.rm = T))

  gcp <- dplyr::select(gcp, LONGITUDE, LAT, quality)
  gcp$quality <- dplyr::if_else(gcp$quality <= -499, NA_real_, gcp$quality)

  pg_tibble <- raster::rasterFromXYZ(gcp, crs = priogrid::prio_crs()) %>%
    priogrid::raster_to_pg() %>%
    priogrid::raster_to_tibble(add_pg_index = T)
  return(pg_tibble)
}
