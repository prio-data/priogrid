
#' Reads the Geocoded Peacekeeping Operations (Geo-PKO) data
#'
#'
#' @return an object of class sf
#' @export
#'
#' @references
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

  # Set date to last day in month
  df$mydate <- lubridate::ym(paste(df$year, df$month, sep = "-")) |> lubridate::rollforward()

  return(df)
}

#' Generate Geo-PKO operations count
#'
#' Takes the count of peacekeeping operations within each PRIO-GRID cell per year
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_geopko_operations_count()
#'
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_operations_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  time_intervals <- pg_date_intervals()
  stack_list <- list()
  for (i in 1:length(time_intervals)) {
    t <- time_intervals[i]
    c <- f |> dplyr::filter(mydate %within% !!t)
    if(nrow(c) > 0){
      r <- terra::rasterize(c, pg, fun = "sum", na.rm = TRUE)
      end_t <- lubridate::int_end(t)
      stack_list[[as.character(end_t)]] <- r
    }

  }

  stack <- terra::rast(stack_list)
  names(stack) <- names(stack_list)
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
#'
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_troops_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  f$no.troops <- ifelse(f$no.troops == "unknown", NA, f$no.troops)
  f$no.troops <- as.numeric(f$no.troops)

  time_intervals <- pg_date_intervals()
  stack_list <- list()
  for (i in 1:length(time_intervals)) {
    t <- time_intervals[i]

    c <- f |> dplyr::filter(mydate %within% !!t)
    if(nrow(c) > 0){
      r <- terra::rasterize(c, pg, field = "no.troops", fun = "sum", na.rm = TRUE)
      end_t <- lubridate::int_end(t)
      stack_list[[as.character(end_t)]] <- r
    }
  }

  stack <- terra::rast(stack_list)
  names(stack) <- names(stack_list)
  return(stack)
}



