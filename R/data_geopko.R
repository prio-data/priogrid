
#' Reads the Geocoded Peacekeeping Operations (Geo-PKO) data
#'
#'
#' @return an object of class sf
#' @export
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}

read_geopko <- function(){
  f <- get_pgfile(source_name = "Geocoded Peacekeeping Operations (Geo-PKO)",
                  source_version = "2.2",
                  id = "7dcbfbfb-9667-4684-af34-85f69fa8d0a0")
  df <- readRDS(f)
  df <- df |>
    dplyr::filter(!is.na(longitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 remove = FALSE) |>
    sf::st_set_crs(4326)

  return(df)
}

#' Generate Geo-PKO sum
#'
#' Takes the number of peacekeeping operations within each PRIO-GRID cell
#'
#' @return
#' @export
#'
#' @examples
#' # r <- gen_geopko_sum()
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}

gen_geopko_sum <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()
  r <- terra::rasterize(terra::vect(f), pg, field = 1, fun = sum, na.rm = TRUE)

  names(r) <- "geopko_sum"
}

#' Generate Geo-PKO count
#'
#' Takes the count of peacekeeping operations within each PRIO-GRID cell per year
#'
#' @return
#' @export
#'
#' @examples
#' # r <- gen_geopko_count()
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}

gen_geopko_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  years <- 1994:2022
  stack_list <- list()

  for (year in years) {
    c <- f |>
      dplyr::group_by(country, year) |>
      dplyr::summarise(count = dplyr::n()) |>
      dplyr::filter(year == !!year)

    r <- terra::rasterize(c, pg, "count")

    stack_list[[as.character(year)]] <- r
  }

  stack <- terra::rast(stack_list)

  names(stack) <- as.character(years)

  return(stack)

}


