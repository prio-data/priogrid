#' gen_era5_rainf_sum
#'
#' @description Generate monthly sum precipitation variable, available from 1950-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_era5_rainf_sum <- function(input_folder, fun = "sum"){

  fnames <- stringr::str_replace(Sys.glob("input/era5/*_totalprecip.nc"), "input/era5/", "")
  
  var <- paste0("era5_rainf_", as.character(fun))
  
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
    ## https://gis.stackexchange.com/questions/395875/converting-units-of-m-day-1-era5-to-mm-cru
    ## https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790
    ## era5 in m, *1000 to get mm, * number of days in month to get mm/month
    dplyr::mutate( !!paste0(var, "_mm_month") := !!as.name(paste0(var))*1000*as.integer(lubridate::days_in_month(yearmonth)))
  
  
  return(era5)
}


#' gen_era5_rainf_sd
#'
#' @description Generate monthly standard deviation precipitation variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_rainf_sd <- function(input_folder){
  gen_era5_rainf_sum(input_folder, fun = "sd")
}


#' gen_era5_rainf_max
#'
#' @description Generate monthly max precipitation variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_rainf_max <- function(input_folder){
  gen_era5_rainf_sum(input_folder, fun = "max")
}


#' gen_era5_rainf_min
#'
#' @description Generate monthly min precipitation variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_rainf_min <- function(input_folder){
  gen_era5_rainf_sum(input_folder, fun = "min")
}


#' gen_era5_rainf_mean
#'
#' @description Generate monthly mean precipitation variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_rainf_mean <- function(input_folder){
  gen_era5_rainf_sum(input_folder, fun = "mean")
}


#' gen_era5_rainf_med
#'
#' @description Generate monthly median precipitation variable, available from 1982-present.
#' 
#' Please cite: Muñoz Sabater, J. (2019): ERA5-Land monthly averaged data from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.68d2bb30 (Accessed on DD-MMM-YYYY)
#' 
#' source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_era5_rainf_med <- function(input_folder){
  gen_era5_rainf_sum(input_folder, fun = "median")
}
