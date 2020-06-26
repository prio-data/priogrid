#' @title epr_excluded
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
                                     variable = "epr_excluded",
                                     need_aggregation = TRUE,
                                     fun = "count")

   return(excluded)

}

#' @title epr_promoted
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
                                     variable = "epr_promoted",
                                     need_aggregation = TRUE,
                                     fun = "count")

   return(promoted)

}

#' @title epr_demoted
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
                                    variable = "epr_demoted",
                                    need_aggregation = TRUE,
                                    fun = "count")

   return(demoted)

}

#' @title epr_regional
#'
#' @description Generate variable that counts the number of regionally based ethnic groups
#' that are settled in a given grid cell in a given year.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_epr_regional <- function(input_folder){
   regional <- prep_epr(input_folder)

   regional <- priogrid::panel_to_pg(regional,
                                     timevar = "year",
                                     variable = "epr_regional",
                                     need_aggregation = TRUE,
                                     fun = "count")

   return(demoted)

}







prep_epr <- function(input_folder){
   files <- file.path(input_folder, "geoepr", "data", c("GeoEPR.shp","EPR-2018.1.1.csv"))

   geoepr <- sf::read_sf(files[1])
   geoepr <- sf::st_transform(geoepr, crs = priogrid::prio_crs()) %>%
     dplyr::filter(type == "Regionally based" | type == "Regional & urban" | type == "Aggregate") %>%
     dplyr::mutate(year = purrr::map2(from, to, `:`)) %>%
     tidyr::unnest(year) %>%
     dplyr::filter(!sf::st_is_empty(geometry))

   epr <- read.csv(files[2], stringsAsFactors = FALSE)

   epr <- epr %>%
     dplyr::mutate(epr_excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
                                          yes = 1, no = 0),
                    year = purrr::map2(from, to, `:`)) %>%
     tidyr::unnest(year) %>%
     dplyr::right_join(geoepr, by = c("year", "gwgroupid", "gwid")) %>%
     dplyr::mutate(status_cat = ifelse(status == "MONOPOLY" | status == "DOMINANT", 1,
                                        ifelse(status == "SENIOR PARTNER" | status == "JUNIOR PARTNER", 2,
                                               ifelse(status == "POWERLESS" | status == "DISCRIMINATED", 3,
                                                      NA)))) %>% # Removing "irrelevant", "self-exclusion", and "state collapse"
     dplyr::arrange(gwgroupid, year) %>%
     dplyr::group_by(gwgroupid) %>%
     dplyr::mutate(lag_status = dplyr::lag(status, n = 1),
                   lag_status_cat = dplyr::lag(status_cat, n = 1)) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(change = ifelse(status_cat != lag_status_cat, 1, 0)) %>%
     dplyr::mutate(epr_promoted = ifelse(as.numeric(lag_status_cat) > as.numeric(status_cat), 1, 0),
                   epr_demoted = ifelse(as.numeric(lag_status_cat) < as.numeric(status_cat), 1, 0)) %>%
     dplyr::mutate(epr_promoted = tidyr::replace_na(epr_promoted, 0),
                   epr_demoted = tidyr::replace_na(epr_demoted, 0)) %>%
     dplyr::mutate(epr_regional = ifelse(type == "Regionally based", yes = 1, no = 0)) %>%
     dplyr::select(gwid, gwgroupid, year, epr_promoted, epr_demoted, epr_excluded, epr_regional, geometry)

   return(epr)


}
