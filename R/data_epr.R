#' @title epr_excluded
#'
#' @description Counts the number of excluded groups (discriminated or powerless), as defined in the GeoEPR/EPR data
#' on the status and location of politically relevant ethnic groups, that are settled in the grid cell in a given year.
#' Group status derived from the EPR Core 2019 dataset, and location derived from the GeoEPR 2019 data. The variable counts only
#' groups that have a distinct subnational settlement pattern.
#'
#' Link to original data: https://icr.ethz.ch/data/epr/
#'
#' Please cite: Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger, Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015. “Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic Power Relations Data Set Family.” Journal of Conflict Resolution 59(7): 1327–42.
#'
#' @param input_folder path to [pg-folder].
#' @param variable one of c("epr_excluded", "epr_promoted", "epr_demoted", "epr_regional")
#'
#' @export
gen_excluded <- function(input_folder, variable = "epr_excluded"){
   excluded <- prep_epr(input_folder)

   excluded <- priogrid::panel_to_pg(excluded,
                                     timevar = "year",
                                     variable = variable,
                                     need_aggregation = TRUE,
                                     fun = "count")

   return(excluded)

}

#' @title epr_promoted
#'
#' @description Counts the number of politically relevant ethnic groups in the grid cell
#' that have increased their political status from the previous year. Group status derived from
#' the EPR Core 2019 dataset, and location derived from the GeoEPR 2019 data.
#' Status defined as group rules alone; shares power; or is excluded.
#'
#' Link to original data: https://icr.ethz.ch/data/epr/
#'
#' Please cite: Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger, Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015. “Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic Power Relations Data Set Family.” Journal of Conflict Resolution 59(7): 1327–42.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_promoted <- function(input_folder){
   gen_excluded(input_folder, variable = "epr_promoted")
}

#' @title epr_demoted
#'
#' @description Counts the number of politically relevant ethnic groups in the grid cell
#' that have decreased their political status from the previous year. Group status derived from
#' the EPR Core 2019 dataset, and location derived from the GeoEPR 2019 data.
#' Status defined as group rules alone; shares power; or is excluded.
#'
#' Link to original data: https://icr.ethz.ch/data/epr/
#'
#' Please cite: Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger, Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015. “Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic Power Relations Data Set Family.” Journal of Conflict Resolution 59(7): 1327–42.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_demoted <- function(input_folder){
   gen_excluded(input_folder, variable = "epr_demoted")
}

#' @title epr_regional
#'
#' @description Counts the number of regionally based ethnic groups
#' that are settled in a given grid cell in a given year; derived from
#' the GeoEPR 2019 data.
#'
#' Link to original data: https://icr.ethz.ch/data/epr/
#'
#' Please cite: Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger, Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015. “Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic Power Relations Data Set Family.” Journal of Conflict Resolution 59(7): 1327–42.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_epr_regional <- function(input_folder){
   gen_excluded(input_folder, variable = "epr_regional")
}



# Data preparation
prep_epr <- function(input_folder){
   files <- file.path(input_folder, "geoepr", "data", c("GeoEPR.shp","EPR-2018.1.1.csv"))

   geoepr <- sf::read_sf(files[1])
   geoepr <- sf::st_transform(geoepr, crs = priogrid::prio_crs()) %>%
     dplyr::filter(type == "Regionally based" | type == "Regional & urban" | type == "Aggregate") %>%
     dplyr::mutate(year = purrr::map2(from, to, `:`)) %>%
     tidyr::unnest(year)

   epr <- read.csv(files[2], stringsAsFactors = FALSE)

   epr <- epr %>%
     dplyr::mutate(epr_excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
                                          yes = 1, no = 0),
                    year = purrr::map2(from, to, `:`)) %>%
     tidyr::unnest(year)

   geoepr <- geoepr %>%
      dplyr::right_join(epr, by = c("year", "gwgroupid", "gwid")) %>%
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
      dplyr::select(gwid, gwgroupid, year, epr_promoted, epr_demoted, epr_excluded, epr_regional, geometry) %>%
      dplyr::filter(!sf::st_is_empty(geometry))

   return(geoepr)
}

