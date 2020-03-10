

# Yearly ------------------------------------------------------------------

#### Note: Takes about 15 mins to run due to for loop in rasterize fun.
#### TODO Check that grid cells identifying groups belong to the same country as the group (gwcode).


#' Generate excluded variable
#'
#' Counts the number of excluded groups of politically relevant ethnic groups
#' settled in a given grid cell in a given year.
#'
#' @param geoepr_data GeoEPR 2018 shapefile.
#' @param epr_data EPR Core 2018 .csv data.

gen_geoepr <- function(path){
   files <- file.path(path,c("GeoEPR.shp","EPR-2018.1.1.csv"))

   geoepr <- sf::st_read(files[1], stringsAsFactors = FALSE, quiet = TRUE)
   geoepr <- sf::st_transform(geoepr, crs = priogrid::prio_crs())
 
   geoepr <- geoepr %>%
      dplyr::select(gwno = gwid, from, to, group, grptype = type, groupid, geometry) %>%
      dplyr::mutate(from = as.numeric(from), to = as.numeric(to),
                    year = priogrid::prio_year(from, to)) %>%
      tidyr::unnest(year)
 
   # Join with EPR to obtain group status
   epr <- read.csv(files[2])
 
   epr <- epr %>%
      dplyr::select(gwno = gwid, groupid, status, from, to) %>%
      dplyr::mutate(excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
                                     yes = 1, no = 0),
                   year = priogrid::prio_year(from, to)) %>%
      tidyr::unnest(year) %>%
      dplyr::select(gwno, groupid, year, excluded)
 
   geoepr_epr <- base::merge(geoepr, epr,
                             by.x = c("gwno", "groupid", "year"),
                             by.y = c("gwno", "groupid", "year"),
                             all.x = TRUE)
 
   geoepr_epr <- geoepr_epr[!sf::st_is_empty(geoepr_epr$geometry),]

   brick <- priogrid::yearly_brick(geoepr_epr, 
                                   variable = "excluded", 
                                   raster.fun = "count")

   return(brick)
}
