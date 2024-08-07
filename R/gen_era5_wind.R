#' gen_era5_wind_sum
#'
#' @description Generate monthly sum near surface air temperature variable, available from 1950-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_era5_wind_sum <- function(input_folder, fun = "sum"){
  
  fnames <- stringr::str_replace(Sys.glob("input/era5/*_2mtemp.nc"), "input/era5/", "")
  
  var <- paste0("era5_wind_", as.character(fun), "_ms")

  era5 <- file.path(input_folder, "input", "era5", fnames)
  
  era5 <- era5 %>% purrr::map(raster::brick) %>%
    raster::stack() %>%
    priogrid::raster_to_pg(aggregation_function = fun) %>%
    priogrid::raster_to_tibble(add_pg_index = TRUE) %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("X1", "X2")),
                        values_to = paste0(var),
                        names_to = "yearmonth") %>%
    dplyr::mutate(yearmonth = lubridate::ymd(stringr::str_replace(yearmonth, "X", ""))) %>%
    dplyr::ungroup() %>%
    ## https://www.metric-conversions.org/speed/meters-per-second-to-kilometers-per-hour.htm?utm_content=cmp-true
    dplyr::mutate( !!paste0("era5_wind_", as.character(fun), "_kmh") := !!as.name(paste0(var))*3.6)
  
  return(era5)
}


#' gen_era5_wind_sd
#'
#' @description Generate monthly standard deviation near surface air temperature variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_wind_sd <- function(input_folder){
  gen_era5_wind_sum(input_folder, fun = "sd")
}


#' gen_era5_wind_max
#'
#' @description Generate monthly max near surface air temperature variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_wind_max <- function(input_folder){
  gen_era5_wind_sum(input_folder, fun = "max")
}


#' gen_era5_wind_min
#'
#' @description Generate monthly min near surface air temperature variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_wind_min <- function(input_folder){
  gen_era5_wind_sum(input_folder, fun = "min")
}


#' gen_era5_wind_mean
#'
#' @description Generate monthly mean near surface air temperature variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_wind_mean <- function(input_folder){
  gen_era5_wind_sum(input_folder, fun = "mean")
}


#' gen_era5_wind_med
#'
#' @description Generate monthly median near surface air temperature variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_wind_med <- function(input_folder){
  gen_era5_wind_sum(input_folder, fun = "median")
}
