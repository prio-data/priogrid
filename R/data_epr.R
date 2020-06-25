#' @title eth_excluded
#'
#' @description Generate variable that counts the number of excluded and politically relevant
#' ethnic groups that are settled in a given grid cell in a given year.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_excluded <- function(input_folder){
   excluded <- prep_epr(input_folder)

   excluded <- priogrid::panel_to_pg(excluded,
                                     timevar = "year",
                                     variable = var,
                                     need_aggregation = TRUE,
                                     fun = "count")

   return(excluded)

}

#' @title eth_promoted
#'
#' @description Generate variable that counts the number of politically relevant ethnic groups
#' that are settled in a given grid cell in a given year, and that have increased their political access
#' from the previous year.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_promoted <- function(input_folder){
   promoted <- prep_epr(input_folder)

   promoted <- priogrid::panel_to_pg(promoted,
                                     timevar = "year",
                                     variable = "eth_promoted",
                                     need_aggregation = TRUE,
                                     fun = "count")

   return(promoted)

}

#' @title eth_demoted
#'
#' @description Generate variable that counts the number of politically relevant ethnic groups
#' that are settled in a given grid cell in a given year, and that have reduced their political access
#' from the previous year.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_demoted <- function(input_folder){
   demoted <- prep_epr(input_folder)

   demoted <- priogrid::panel_to_pg(demoted,
                                    timevar = "year",
                                    variable = "eth_demoted",
                                    need_aggregation = TRUE,
                                    fun = "count")

   return(demoted)

}



prep_epr <- function(input_folder){
   files <- file.path(input_folder, "geoepr", "data", c("GeoEPR.shp","EPR-2018.1.1.csv"))

   geoepr <- sf::read_sf(files[1])
   geoepr <- sf::st_transform(geoepr, crs = priogrid::prio_crs())

   epr <- read.csv(files[2], stringsAsFactors = FALSE)

   epr <- epr %>%
      dplyr::mutate(eth_excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
                                          yes = 1, no = 0),
                    year = purrr::map2(from, to, `:`)) %>%
      tidyr::unnest(year) %>%
      dplyr::select(gwgroupid, year, status, eth_excluded)

   geoepr <- geoepr %>%
      dplyr::mutate(year = purrr::map2(from, to, `:`)) %>%
      tidyr::unnest(year) %>%
      dplyr::left_join(epr, by = c("year", "gwgroupid"))

   geoepr <- geoepr %>%
      dplyr::mutate(status_cat = ifelse(status == "MONOPOLY" | status == "DOMINANT", 1,
                                        ifelse(status == "SENIOR PARTNER" | status == "JUNIOR PARTNER", 2,
                                               ifelse(status == "POWERLESS" | status == "DISCRIMINATED", 3,
                                                      0)))) %>%
      dplyr::filter(status_cat != 0) %>% # Removing "irrelevant", "self-exclusion", and "state collapse"
      dplyr::arrange(gwgroupid, year) %>%
      dplyr::group_by(gwgroupid) %>%
      dplyr::mutate(lag_status = lag(status, n = 1),
                    lag_status_cat = lag(status_cat, n = 1)) %>%
      dplyr::mutate(change = ifelse(status_cat != lag_status_cat, 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(eth_promoted = ifelse(as.numeric(lag_status_cat) > as.numeric(status_cat), 1, 0),
                    eth_demoted = ifelse(as.numeric(lag_status_cat) < as.numeric(status_cat), 1, 0)) %>%
      dplyr::mutate(eth_promoted = tidyr::replace_na(eth_promoted, 0),
                    eth_demoted = tidyr::replace_na(eth_demoted, 0)) %>%
      dplyr::select(gwid, year, eth_promoted, eth_demoted, eth_excluded)

   geoepr <- geoepr[!sf::st_is_empty(geoepr$geometry), ]

   return(geoepr)


}
