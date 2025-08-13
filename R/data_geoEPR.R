#' Reads the GeoEPR - Geo-referencing Ethnic Power Relations data
#'
#' Formats gwsdate and gwedate as date objects, and adds a utility column date_interval
#'
#' @return an object of class sf
#' @export
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{priogrid}
#' \insertRef{vogtIntegratingDataEthnicity2015}{priogrid}
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

#' Reads the EPR Core 2023 data
#'
#'
#' @return an object of class tibble
#' @export
#'
#' @references
#' \insertRef{vogtIntegratingDataEthnicity2015}{priogrid}
read_epr <- function() {
  f <- get_pgfile(source_name = "ETH ICR EPR Core",
                   source_version = "2023",
                   id = "287bfdf7-2f4f-402a-88df-5fe1f8b7046b")

  df <- readr::read_csv(f)

  df <- df |>
    dplyr::mutate(gwsdate = as.Date(paste0(from, "-01-01")),
                  gwedate = as.Date(paste0(to, "-12-31"))) |>
    dplyr::mutate(date_interval = lubridate::interval(gwsdate, gwedate))

  return(df)
}

#' PRIO-GRID cells with any regionally based politically excluded groups
#'
#' @param excluded A character vector with the EPR status codes to include in the "excluded" group.
#'  The status codes in EPR is "MONOPOLY", "DISCRIMINATED", "POWERLESS", "DOMINANT", "SENIOR PARTNER",
#'  "JUNIOR PARTNER", "IRRELEVANT", "SELF-EXCLUSION", and "STATE COLLAPSE"
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_geoepr_reg_excluded()
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{priogrid}
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{priogrid}
gen_geoepr_reg_excluded <- function(excluded = c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION")) {

  geoepr <- read_geoepr() |> dplyr::filter(type == "Regionally based") |>
    dplyr::select(from, to, gwgroupid, geometry)
  epr <- read_epr() |> dplyr::filter(status %in% excluded)
  pg <- prio_blank_grid()

  df <- dplyr::inner_join(geoepr, epr, by = c("gwgroupid", "from", "to"))

  df <- sf::st_transform(df, crs = sf::st_crs(pg))
  pg_intervals <- pg_date_intervals()
  pg_intervals <- pg_intervals[lubridate::int_start(pg_intervals) >= min(lubridate::int_start(df$date_interval))]
  pg_intervals <- pg_intervals[lubridate::int_end(pg_intervals) <= max(lubridate::int_end(df$date_interval))]

  make_raster <- function(current_interval, pg, df){
    sdf <- df |> dplyr::filter(lubridate::int_overlaps(date_interval, current_interval))

    if(nrow(sdf) > 0){
      sdf <- sdf |> dplyr::summarize(geometry = sf::st_combine(geometry))
      pg <- prio_blank_grid()
      coversh <- exactextractr::exact_extract(pg, sdf)
      ra <- exactextractr::rasterize_polygons(sdf, pg)
      pg <- pg*ra # Remove non-land cells
      res <- terra::classify(pg, coversh[[1]])
      return(res)
    }
  }

  r <- make_raster(pg_intervals[1], pg, df)
  for(i in 2:length(pg_intervals)){
    terra::add(r) <- make_raster(pg_intervals[i], pg, df)
  }
  names(r) <- as.character(pg_intervals)
  return(r)
}

