
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

  pgday <- pg_dates()[1] |> lubridate::day()
  df$date <- lubridate::ymd(paste(df$year, df$month, pgday, sep = "-")) |> as.character()

  return(df)
}

#' Generate Geo-PKO count
#'
#' Takes the count of peacekeeping operations within each PRIO-GRID cell per year
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_geopko_operations_count()
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_operations_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  years <- as.numeric(1994:2022)
  stack_list <- list()


  for (year in years) {
    print(paste("Year:", year))
    c <- f |>
      dplyr::filter(year == !!year)

    r <- terra::rasterize(c, pg, fun = "sum", na.rm = TRUE)


    stack_list[[as.character(year)]] <- r
    print(terra::minmax(r))
  }

  stack <- terra::rast(stack_list)

  names(stack) <- as.character(years)

  return(stack)

}

#' Generate the number of troops from Geo-PKO
#'
#' Takes the number of peace keeping troops within each PRIO-GRID cell
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # t <- gen_pko_troops_count
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_troops_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  f$no.troops <- ifelse(f$no.troops == "unknown", NA, f$no.troops)
  f$no.troops <- as.numeric(f$no.troops)

  years <- as.numeric(1994:2022)
  stack_list <- list()

  for (year in years) {
    print(paste("Year:", year))
    c <- f |>
      dplyr::filter(year == !!year)

    t <- terra::rasterize(c, pg, field = "no.troops", fun = "sum", na.rm = TRUE)


    stack_list[[as.character(year)]] <- t
    print(terra::minmax(t))
  }

  stack <- terra::rast(stack_list)

  names(stack) <- as.character(years)

  return(stack)

}



