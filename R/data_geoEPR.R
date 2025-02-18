#' Reads the GeoEPR - Geo-referencing Ethnic Power Relations data
#'
#' Formats gwsdate and gwedate as date objects, and adds a utility column date_interval
#'
#' @return an object of class sf
#' @export
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{vogtIntegratingDataEthnicity2015}{priogrid}
read_geoEPR <- function() {
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

#' Generate geoEPR sum by group
#'
#' Takes the sum of politically relevant ethnic groups
#' within each PRIO-GRID cell per year
#'
#' @return SpatRast
#'
#' @param from A single year as start year
#' @param to A single year as end year
#' @param group_name Name of politically relevant ethnic group
#'
#' @export
#'
#' @examples
#' # r <- gen_geoEPR(from = 1946, to = 1958, group_name = "Whites")
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{vogtIntegratingDataEthnicity2015}{priogrid}
gen_geoEPR <- function(from = NULL, to = NULL, group_name = NULL) {

  geoEPR_sf <- read_geoEPR()
  pg <- prio_blank_grid()

  if (!is.null(from) & !is.null(to)) {
    geoEPR_sf <- geoEPR_sf |> dplyr::filter(from <= to, to >= from)
  }

  if (!is.null(group_name)) {
    geoEPR_sf <- geoEPR_sf |> dplyr::filter(group %in% group_name)
  }

  unique_groups <- unique(geoEPR_sf$group)

  raster_list <- list()

  for (grp in unique_groups) {
    group_sf <- geoEPR_sf |> dplyr::filter(group == grp)

    pg_r <- pg

    extracted_values <- exactextractr::exact_extract(pg_r, group_sf, function(x, w) sum(w, na.rm = TRUE), progress = FALSE)

    values(pg_r) <- extracted_values

    raster_list[[grp]] <- pg_r
  }

  raster_stack <- terra::rast(raster_list)
  names(raster_stack) <- unique_groups
  return(raster_stack)
}

