#' gen_nl_bm_sum
#'
#' @description Generate monthly sum nighttime lights variable, available from 2012-present.
#' 
#' Please cite: Román, M.O., Wang, Z., Sun, Q., Kalb, V., Miller, S.D., Molthan, A., Schultz, L., Bell, J., Stokes, E.C., Pandey, B. and Seto, K.C., et al. (2018). NASA's Black Marble nighttime lights product suite. Remote Sensing of Environment 210, 113-143. doi:10.1016/j.rse.2018.03.017.
#' 
#' source: https://blackmarble.gsfc.nasa.gov/ & https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_nl_bm_sum <- function(input_folder, fun = "sum"){
  
  fnames <- stringr::str_replace(Sys.glob("input/nl_black_marble/*.tif"), "input/nl_black_marble/", "")
  
  var <- paste0("nl_bm_", as.character(fun))
  
  nl_bm <- file.path(input_folder, "input", "nl_black_marble", fnames) %>% 
    purrr::map(raster::raster) %>%
    purrr::map(priogrid::raster_to_pg, aggregation_function = fun) %>% 
    purrr::map(priogrid::raster_to_tibble, add_pg_index = TRUE) %>%
    purrr::map_dfr(tidyr::pivot_longer, cols = 3, names_to = "yearmonth", values_to = paste0(var)) %>%
    dplyr::mutate(yearmonth = lubridate::ym(stringr::str_replace(yearmonth, "VNP46A3_t", "")))
    
  return(nl_bm)
}


#' gen_nl_bm_sd
#'
#' @description Generate monthly standard deviation of nighttime lights variable, available from 2012-present.
#' 
#' Please cite: Román, M.O., Wang, Z., Sun, Q., Kalb, V., Miller, S.D., Molthan, A., Schultz, L., Bell, J., Stokes, E.C., Pandey, B. and Seto, K.C., et al. (2018). NASA's Black Marble nighttime lights product suite. Remote Sensing of Environment 210, 113-143. doi:10.1016/j.rse.2018.03.017.
#' 
#' source: https://blackmarble.gsfc.nasa.gov/ & https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_bm_sd <- function(input_folder){
  gen_nl_bm_sum(input_folder, fun = "sd")
}


#' gen_nl_bm_max
#'
#' @description Generate monthly max nighttime lights variable, available from 2012-present.
#' 
#' Please cite: Román, M.O., Wang, Z., Sun, Q., Kalb, V., Miller, S.D., Molthan, A., Schultz, L., Bell, J., Stokes, E.C., Pandey, B. and Seto, K.C., et al. (2018). NASA's Black Marble nighttime lights product suite. Remote Sensing of Environment 210, 113-143. doi:10.1016/j.rse.2018.03.017.
#' 
#' source: https://blackmarble.gsfc.nasa.gov/ & https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_bm_max <- function(input_folder){
  gen_nl_bm_sum(input_folder, fun = "max")
}


#' gen_nl_bm_min
#'
#' @description Generate monthly min nighttime lights variable, available from 2012-present.
#' 
#' Please cite: Román, M.O., Wang, Z., Sun, Q., Kalb, V., Miller, S.D., Molthan, A., Schultz, L., Bell, J., Stokes, E.C., Pandey, B. and Seto, K.C., et al. (2018). NASA's Black Marble nighttime lights product suite. Remote Sensing of Environment 210, 113-143. doi:10.1016/j.rse.2018.03.017.
#' 
#' source: https://blackmarble.gsfc.nasa.gov/ & https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_bm_min <- function(input_folder){
  gen_nl_bm_sum(input_folder, fun = "min")
}


#' gen_nl_bm_mean
#'
#' @description Generate monthly mean nighttime lights variable, available from 2012-present.
#' 
#' Please cite: Román, M.O., Wang, Z., Sun, Q., Kalb, V., Miller, S.D., Molthan, A., Schultz, L., Bell, J., Stokes, E.C., Pandey, B. and Seto, K.C., et al. (2018). NASA's Black Marble nighttime lights product suite. Remote Sensing of Environment 210, 113-143. doi:10.1016/j.rse.2018.03.017.
#' 
#' source: https://blackmarble.gsfc.nasa.gov/ & https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_bm_mean <- function(input_folder){
  gen_nl_bm_sum(input_folder, fun = "mean")
}


#' gen_nl_bm_med
#'
#' @description Generate monthly median nighttime lights variable, available from 2012-present.
#' 
#' Please cite: Román, M.O., Wang, Z., Sun, Q., Kalb, V., Miller, S.D., Molthan, A., Schultz, L., Bell, J., Stokes, E.C., Pandey, B. and Seto, K.C., et al. (2018). NASA's Black Marble nighttime lights product suite. Remote Sensing of Environment 210, 113-143. doi:10.1016/j.rse.2018.03.017.
#' 
#' source: https://blackmarble.gsfc.nasa.gov/ & https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_nl_bm_med <- function(input_folder){
  gen_nl_bm_sum(input_folder, fun = "median")
}
