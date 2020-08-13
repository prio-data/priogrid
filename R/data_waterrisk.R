#' @title waterrisk
#'
#' @description Generate variable measuring overall waterrisk, drawn from the yearly Aqueduct 3.0 dataset.
#' Please cite: Hofste, R., S. Kuzma, S. Walker, E.H., Sutanudjaja, et. al. 2019. “Aqueduct 3.0: Updated Decision-Relevant Global Water Risk Indicators.” Technical Note. Washington, DC: World Resources Institute. Available online at: https://www.wri.org/publication/aqueduct-30.
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_waterrisk <- function(input_folder){
  waterrisk <- sf::read_sf(file.path(input_folder, "waterrisk", "Y2019M07D12_Aqueduct30_V01", "baseline",
                                     "annual", "y2019m07d11_aqueduct30_annual_v01.gpkg"))

  waterrisk <- waterrisk %>%
    dplyr::select(waterrisk = bws_cat) %>%
    dplyr::mutate(waterrisk = dplyr::na_if(waterrisk, -1))

  waterrisk_sum <- priogrid::vector_to_pg(waterrisk, variable = "waterrisk", need_aggregation = TRUE, fun = "sum")
  waterrisk_sum <- priogrid::raster_to_tibble(waterrisk_sum, add_pg_index = T)

  waterrisk_count <- priogrid::vector_to_pg(waterrisk, variable = "waterrisk", need_aggregation = TRUE, fun = "count")
  waterrisk_count <- priogrid::raster_to_tibble(waterrisk_count, add_pg_index = TRUE)

  waterrisk_count <- waterrisk_count %>% dplyr::rename("count" = "waterrisk")

  waterrisk <- dplyr::left_join(waterrisk_sum, waterrisk_count, by = c("x", "y", "pgid"))
  waterrisk <- waterrisk %>%
    dplyr::mutate(waterrisk = waterrisk/count)

  waterrisk$count <- NULL

  waterrisk <- priogrid::interpolate_crossection(waterrisk, variable = "waterrisk", lon = "x", lat = "y",
                                                 input_folder = input_folder)

  return(waterrisk)
}

