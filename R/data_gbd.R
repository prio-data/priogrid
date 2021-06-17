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
#'
#' @export
gen_gbd_wasting <- function(input_folder){
  gbd_files <- list.files(file.path(input_folder, "gbd", "data"), full.names = T)

  wasting_files <- gbd_files[grepl("WASTING", gbd_files)]

  wasting <- raster::brick(lapply(wasting_files, raster::raster))
  names(wasting) <- paste("year_", sapply(stringr::str_split(names(wasting), "_"), function(x) x[12]), sep = "")

  wasting <- priogrid::raster_to_pg(wasting, aggregation_function = "mean")
  wasting <- priogrid::raster_to_tibble(wasting, add_pg_index = TRUE)
  wasting <- wasting %>% pivot_longer(cols = starts_with("year_"), names_prefix = "year_", names_to = "year", values_to = "wasting")
  return(wasting)
}
