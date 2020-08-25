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


#' @title waterrisk monthly
#'
#' @description Generate variable measuring monthly waterrisk, drawn from the monthly Aqueduct 3.0 dataset.
#' Please cite: Hofste, R., S. Kuzma, S. Walker, E.H., Sutanudjaja, et. al. 2019. “Aqueduct 3.0: Updated Decision-Relevant Global Water Risk Indicators.” Technical Note. Washington, DC: World Resources Institute. Available online at: https://www.wri.org/publication/aqueduct-30.
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_waterrisk_monthly <- function(input_folder){
  waterrisk <- sf::read_sf(file.path(input_folder, "waterrisk", "Y2019M07D12_Aqueduct30_V01", "baseline",
                                   "monthly", "y2019m07d12_rh_aqueduct30_data_download_monthly_v01.gpkg"))
  waterrisk_long <- waterrisk %>%
    as.data.frame() %>%
    dplyr::select(pfaf_id,
                  bws_01_cat,
                  bws_02_cat,
                  bws_03_cat,
                  bws_04_cat,
                  bws_05_cat,
                  bws_06_cat,
                  bws_07_cat,
                  bws_08_cat,
                  bws_09_cat,
                  bws_10_cat,
                  bws_11_cat,
                  bws_12_cat) %>%
    tidyr::pivot_longer(cols = bws_01_cat:bws_12_cat,
                        names_to = "month",
                        values_to = "waterrisk")

  waterrisk <- waterrisk %>%
    dplyr::select(pfaf_id) %>%
    dplyr::full_join(waterrisk_long, by = "pfaf_id") %>%
    dplyr::mutate(month = readr::parse_number(as.character(month)),
                  waterrisk = dplyr::na_if(waterrisk, -1),
                  waterrisk = dplyr::na_if(waterrisk, -9999)) %>%
    dplyr::filter(!is.na(waterrisk))

  full_pg <- tibble::tibble()
  for(i in seq(1, 12, 1)){
    month_df <- waterrisk %>% dplyr::filter(month == i)

    df_sum <- priogrid::vector_to_pg(month_df, variable = "waterrisk", fun = "sum")
    temp_sum <- priogrid::raster_to_tibble(df_sum, add_pg_index = TRUE) %>%
      dplyr::mutate(month = i)

    df_count <- priogrid::vector_to_pg(month_df, variable = "waterrisk", fun = "count")
    temp_count <- priogrid::raster_to_tibble(df_count, add_pg_index = TRUE) %>%
      dplyr::mutate(month = i) %>%
      dplyr::rename("count" = "waterrisk")

    temp <- dplyr::left_join(temp_sum, temp_count, by = c("x", "y", "pgid", "month"))
    temp <- temp %>%
      dplyr::mutate(waterrisk = waterrisk/count)

    temp_ipol <- priogrid::interpolate_crossection(temp, variable = "waterrisk",
                                                   lon = "x", lat = "y", input_folder = input_folder)
    temp_ipol <- temp_ipol %>% dplyr::mutate(month = i)

    full_pg <- dplyr::bind_rows(full_pg, temp_ipol)
    }

  return(full_pg)
}
