#' gen_fldas_wind_sum
#'
#' @description Generate monthly sum near surface wind variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' Please cite: McNally, Amy, Jacob, Jossy, Arsenault, Kristi, Slinski, Kimberly, Sarmiento, Daniel P., Hoell, Andrew, Pervez, Shahriar, Rowland, James, Budde, Mike, Kumar, Sujay, Peters-Lidard, Christa, Verdin, James P.. 2022.  A Central Asia hydrologic monitoring dataset for food and water security applications in Afghanistan. Earth System Science Data. Vol. 14, No. 7, pp. 3115-3135. DOI: 10.5194/essd-14-3115-2022  ISSN: 1866-3508.
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_fldas_wind_sum <- function(input_folder, fun = "sum"){
  
  fnames <- stringr::str_replace(Sys.glob("input/fldas_noah/*.001.nc"), "input/fldas_noah/", "")
  
  var <- paste0("fldas_wind_", as.character(fun), "_ms")
  
  fldas <- file.path(input_folder, "input", "fldas_noah", fnames)
  
  fldas <- fldas %>% purrr::map(raster::brick, varname = "Wind_f_tavg") %>%
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
    dplyr::mutate( !!paste0("fldas_wind_", as.character(fun), "_kmh") := !!as.name(paste0(var))*3.6)
  
  return(fldas)
}


#' gen_fldas_wind_sd
#'
#' @description Generate monthly standard deviation near surface wind variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_wind_sd <- function(input_folder){
  gen_fldas_wind_sum(input_folder, fun = "sd")
}


#' gen_fldas_wind_max
#'
#' @description Generate monthly max near surface wind variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_wind_max <- function(input_folder){
  gen_fldas_wind_sum(input_folder, fun = "max")
}


#' gen_fldas_wind_min
#'
#' @description Generate monthly min near surface wind variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_wind_min <- function(input_folder){
  gen_fldas_wind_sum(input_folder, fun = "min")
}


#' gen_fldas_wind_mean
#'
#' @description Generate monthly mean near surface wind variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_wind_mean <- function(input_folder){
  gen_fldas_wind_sum(input_folder, fun = "mean")
}


#' gen_fldas_wind_med
#'
#' @description Generate monthly median near surface wind variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_wind_med <- function(input_folder){
  gen_fldas_wind_sum(input_folder, fun = "median")
}
