
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

gen_geopko_mean <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()
  r <- terra::rasterize(f, pg, field = 1, fun = mean, na.rm = TRUE)
}


gen_geopko_min <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()
  r <- terra::rasterize(f, pg, field = 1, fun = min, na.rm = TRUE)
}

gen_geopko_max <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()
  r <- terra::rasterize(f, pg, field = 1, fun = max, na.rm = TRUE)
}

gen_geopko_sd <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()
  r <- terra::rasterize(f, pg, field = 1, fun = sd, na.rm = TRUE)
}
