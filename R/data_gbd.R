#' @title gbd_wasting
#'
#' @description Generate mean wasting
#' for years 2000 - 2019 using data from
#' the IHME Low- and Middle-Income Country Child Growth Failure Geospatial Estimates 2000-2017.
#'
#' Link to original data: http://ghdx.healthdata.org/record/ihme-data/lmic-child-growth-failure-geospatial-estimates-2000-2017.
#'
#' Please cite: Institute for Health Metrics and Evaluation (IHME). Low- and Middle-Income Country Child Growth Failure Geospatial Estimates 2000-2017. Seattle, United States of America: Institute for Health Metrics and Evaluation (IHME), 2020.
#'
#' @param input_folder path to [pg-folder].
#' @param wasting if `TRUE`, calculate wasting data, else calculate stunting data.
#'
#' @export
gen_gbd_wasting <- function(input_folder, wasting = TRUE){
  gbd_files <- list.files(file.path(input_folder, "gbd", "data"), full.names = T)

  if(wasting){
    gbd_files <- gbd_files[grepl("WASTING", gbd_files)]
  } else {
    gbd_files <- gbd_files[grepl("STUNTING", gbd_files)]
  }

  r <- raster::brick(lapply(gbd_files, raster::raster))
  names(r) <- paste("year_", sapply(stringr::str_split(names(r), "_"), function(x) x[12]), sep = "")

  r <- priogrid::raster_to_pg(r, aggregation_function = "mean")
  df <- priogrid::raster_to_tibble(r, add_pg_index = TRUE)

  if(wasting){
    df <- df %>% tidyr::pivot_longer(cols = starts_with("year_"), names_prefix = "year_", names_to = "year", values_to = "wasting")
  } else{
    df <- df %>% tidyr::pivot_longer(cols = starts_with("year_"), names_prefix = "year_", names_to = "year", values_to = "stunting")
  }
  return(df)
}

#' @title gbd_stunting
#'
#' @description Generate mean stunting
#' for years 2000 - 2017 using data from
#' the IHME Low- and Middle-Income Country Child Growth Failure Geospatial Estimates 2000-2017.
#'
#' Link to original data: http://ghdx.healthdata.org/record/ihme-data/lmic-child-growth-failure-geospatial-estimates-2000-2017.
#'
#' Please cite: Institute for Health Metrics and Evaluation (IHME). Low- and Middle-Income Country Child Growth Failure Geospatial Estimates 2000-2017. Seattle, United States of America: Institute for Health Metrics and Evaluation (IHME), 2020.
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_gbd_stunting <- function(input_folder, wasting = FALSE){
  priogrid::gen_gbd_wasting(input_folder, wasting)
}
