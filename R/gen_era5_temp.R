#' gen_era5_temp_sum
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
gen_era5_temp_sum <- function(input_folder, fun = "sum"){
  
  fnames <- stringr::str_replace(Sys.glob("input/era5/*_2mtemp.nc"), "input/era5/", "")
  
  if (fun != "sd") {
    var <- paste0("era5_temp_", as.character(fun), "_K")
  } else {
    var <- paste0("era5_temp_", as.character(fun))
  }

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
    dplyr::ungroup()
  
  ## convert from Kelvin to Celsius
  if (fun != "sd") {
    varC <- paste0("era5_temp_", as.character(fun), "_C")
    
    era5 <- era5 %>%
      dplyr::mutate( !!paste0(varC) := !!as.name(paste0(var))-273.15)
  }
  
  return(era5)
}


#' gen_era5_temp_sd
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
gen_era5_temp_sd <- function(input_folder){
  gen_era5_temp_sum(input_folder, fun = "sd")
}


#' gen_era5_temp_max
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
gen_era5_temp_max <- function(input_folder){
  gen_era5_temp_sum(input_folder, fun = "max")
}


#' gen_era5_temp_min
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
gen_era5_temp_min <- function(input_folder){
  gen_era5_temp_sum(input_folder, fun = "min")
}


#' gen_era5_temp_mean
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
gen_era5_temp_mean <- function(input_folder){
  gen_era5_temp_sum(input_folder, fun = "mean")
}


#' gen_era5_temp_med
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
gen_era5_temp_med <- function(input_folder){
  gen_era5_temp_sum(input_folder, fun = "median")
}
