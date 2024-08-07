#' gen_fldas_temp_rad_sum
#'
#' @description Generate monthly sum surface radiation temperature variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_fldas_temp_rad_sum <- function(input_folder, fun = "sum"){

  fnames <- str_replace(Sys.glob("input/fldas_noah/*.001.nc"), "input/fldas_noah/", "")

  if (fun != "sd") {
    var <- paste0("fldas_temp_rad_", as.character(fun), "_K")
  } else {
    var <- paste0("fldas_temp_rad_", as.character(fun))
  }

  fldas <- file.path(input_folder, "input", "fldas_noah", fnames)
  
  fldas <- fldas %>% purrr::map(raster::brick, varname = "RadT_tavg") %>%
    raster::stack() %>%
    priogrid::raster_to_pg(aggregation_function = fun) %>%
    priogrid::raster_to_tibble(add_pg_index = TRUE) %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("X1", "X2")),
                        values_to = paste0(var),
                        names_to = "yearmonth") %>%
    dplyr::mutate(yearmonth = lubridate::ymd(stringr::str_replace(yearmonth, "X", ""))) %>%
    dplyr::ungroup()
  
  
  varC <- paste0("fldas_temp_rad_", as.character(fun), "_C")
  ## convert from Kelvin to Celsius
  if (fun != "sd") {
    fldas <- fldas %>%
      dplyr::mutate( !!paste0(varC) := !!as.name(paste0(var))-273.15)
  }
  
  return(fldas)
}


#' gen_fldas_temp_rad_sd
#'
#' @description Generate monthly standard deviation surface radiation temperature variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_temp_rad_sd <- function(input_folder){
  gen_fldas_temp_rad_sum(input_folder, fun = "sd")
}


#' gen_fldas_temp_rad_max
#'
#' @description Generate monthly max surface radiation temperature variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_temp_rad_max <- function(input_folder){
  gen_fldas_temp_rad_sum(input_folder, fun = "max")
}


#' gen_fldas_temp_rad_min
#'
#' @description Generate monthly min surface radiation temperature variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_temp_rad_min <- function(input_folder){
  gen_fldas_temp_rad_sum(input_folder, fun = "min")
}


#' gen_fldas_temp_rad_mean
#'
#' @description Generate monthly mean surface radiation temperature variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_temp_rad_mean <- function(input_folder){
  gen_fldas_temp_rad_sum(input_folder, fun = "mean")
}


#' gen_fldas_temp_rad_med
#'
#' @description Generate monthly median surface radiation temperature variable, available from 1982-present.
#' 
#' Please cite: Amy McNally NASA/GSFC/HSL (2018), FLDAS Noah Land Surface Model L4 Global Monthly 0.1 x 0.1 degree (MERRA-2 and CHIRPS), Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/5NHC22T9375G
#' 
#' source: https://disc.gsfc.nasa.gov/datasets/FLDAS_NOAH01_C_GL_M_001/summary
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_fldas_temp_rad_med <- function(input_folder){
  gen_fldas_temp_rad_sum(input_folder, fun = "median")
}
