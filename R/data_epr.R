#' @title excluded
#'
#' @description Generate variable that counts the number of excluded and politically relevant
#' ethnic groups that are settled in a given grid cell in a given year.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_excluded <- function(input_folder){
   files <- file.path(input_folder, "geoepr", "data", c("GeoEPR.shp","EPR-2018.1.1.csv"))

   geoepr <- sf::read_sf(files[1])
   geoepr <- sf::st_transform(geoepr, crs = priogrid::prio_crs())

   geoepr <- geoepr %>%
      dplyr::select(gwno = gwid, from, to, group, grptype = type, groupid, geometry) %>%
      dplyr::mutate(from = as.numeric(from), to = as.numeric(to),
                    year = purrr::map2(from, to, `:`)) %>%
      tidyr::unnest(year)

   # Join with EPR to obtain group status
   epr <- read.csv(files[2])

   epr <- epr %>%
      dplyr::select(gwno = gwid, groupid, status, from, to) %>%
      dplyr::mutate(excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
                                     yes = 1, no = 0),
                   year = purrr::map2(from, to, `:`)) %>%
      tidyr::unnest(year) %>%
      dplyr::select(gwno, groupid, year, excluded)

   geoepr_epr <- base::merge(geoepr, epr,
                             by.x = c("gwno", "groupid", "year"),
                             by.y = c("gwno", "groupid", "year"),
                             all.x = TRUE)

   geoepr_epr <- geoepr_epr[!sf::st_is_empty(geoepr_epr$geometry),]

   epr_pg <- priogrid::panel_to_pg(geoepr_epr,
                                timevar = "year",
                                variable = "excluded",
                                need_aggregation = TRUE,
                                fun = "count")

   return(epr_pg)
}

