#' gen_fldas_rainf_sum
#'
#' @description Generate monthly sum rainfall variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' Please cite: McNally, Amy, Jacob, Jossy, Arsenault, Kristi, Slinski, Kimberly, Sarmiento, Daniel P., Hoell, Andrew, Pervez, Shahriar, Rowland, James, Budde, Mike, Kumar, Sujay, Peters-Lidard, Christa, Verdin, James P.. 2022.  A Central Asia hydrologic monitoring dataset for food and water security applications in Afghanistan. Earth System Science Data. Vol. 14, No. 7, pp. 3115-3135. DOI: 10.5194/essd-14-3115-2022  ISSN: 1866-3508.
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_fldas_rainf_sum <- function(input_folder, fun = "sum"){

  fnames <- stringr::str_replace(Sys.glob("input/fldas_noah/*.001.nc"), "input/fldas_noah/", "")
  
  var <- paste0("fldas_rainf_", as.character(fun))
  
  fldas <- file.path(input_folder, "input", "fldas_noah", fnames)
  
  fldas <- fldas %>% purrr::map(raster::brick, varname = "Rainf_f_tavg") %>%
    raster::stack() %>%
    priogrid::raster_to_pg(aggregation_function = fun) %>%
    priogrid::raster_to_tibble(add_pg_index = TRUE) %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("X1", "X2")),
                        values_to = paste0(var),
                        names_to = "yearmonth") %>%
    dplyr::mutate(yearmonth = lubridate::ymd(stringr::str_replace(yearmonth, "X", ""))) %>%
    dplyr::ungroup() %>%
    ## https://gis.stackexchange.com/questions/403692/gee-converting-units-of-the-fldas-precipitation-data-rainf-f-tavg-from-kg-m-2
    ## https://github.com/r-quantities/units/issues/129 
    ## *84000 [24*60*60] to get mm/day; * number of days to get mm/month
    dplyr::mutate( !!paste0(var, "_mm_month") := !!as.name(paste0(var))*86400*as.integer(lubridate::days_in_month(yearmonth)))
  
  return(fldas)
}


#' gen_fldas_rainf_sd
#'
#' @description Generate monthly standard deviation rainfall variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_rainf_sd <- function(input_folder){
  gen_fldas_rainf_sum(input_folder, fun = "sd")
}


#' gen_fldas_rainf_max
#'
#' @description Generate monthly max rainfall variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_rainf_max <- function(input_folder){
  gen_fldas_rainf_sum(input_folder, fun = "max")
}


#' gen_fldas_rainf_min
#'
#' @description Generate monthly min rainfall variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_rainf_min <- function(input_folder){
  gen_fldas_rainf_sum(input_folder, fun = "min")
}


#' gen_fldas_rainf_mean
#'
#' @description Generate monthly mean rainfall variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_rainf_mean <- function(input_folder){
  gen_fldas_rainf_sum(input_folder, fun = "mean")
}


#' gen_fldas_rainf_med
#'
#' @description Generate monthly median rainfall variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_rainf_med <- function(input_folder){
  gen_fldas_rainf_sum(input_folder, fun = "median")
}
