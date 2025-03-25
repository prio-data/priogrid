#' Reads the GeoEPR - Geo-referencing Ethnic Power Relations data
#'
#' Formats gwsdate and gwedate as date objects, and adds a utility column date_interval
#'
#' @return an object of class sf
#' @export
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{vogtIntegratingDataEthnicity2015}{priogrid}
read_geoepr <- function() {
  f <- get_pgfile(source_name = "ETH ICR GeoEPR",
                  source_version = "2023",
                  id = "3900b527-a728-4c26-b0ab-f4441d3ee2e8")

  df <- sf::st_read(f)

  df <- df |>
    dplyr::mutate(gwsdate = as.Date(paste0(from, "-01-01")),
           gwedate = as.Date(paste0(to, "-12-31"))) |>
    dplyr::mutate(date_interval = lubridate::interval(gwsdate, gwedate)) |>
    dplyr::filter(!sf::st_is_empty(geometry))

  return(df)
}

#' PRIO-GRID cells with any regionally based politically excluded groups
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_geo_excluded()
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{vogtIntegratingDataEthnicity2015}{priogrid}
gen_geoepr_excluded <- function() {

  geoepr <- read_geoepr()
  pg <- prio_blank_grid()


  # raster_stack <- terra::rast(raster_list)
  # names(raster_stack) <- unique_groups
  # return(raster_stack)
}

