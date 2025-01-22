
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
    sf::st_set_crs(4326) |>
    sf::st_transform(3857)

  return(df)
}

#' Generate Geo-PKO count
#'
#' Takes the count of peacekeeping operations within each PRIO-GRID cell per year
#'
#' @return
#' @export
#'
#' @examples
#' # r <- gen_geopko_operations_count()
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_operations_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  years <- 1994:2022
  stack_list <- list()


  for (year in years) {
    print(paste("Year:", year))
    c <- f |>
      dplyr::group_by(country, year) |>
      dplyr::summarise(count = dplyr::n())

    r <- terra::rasterize(c, pg, fun = sum, na.rm = TRUE)


    stack_list[[as.character(year)]] <- r
    print(terra::minmax(r))
  }

  stack <- terra::rast(stack_list)

  names(stack) <- as.character(years)

  return(stack)

}


