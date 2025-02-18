

read_geoEPR <- function() {
  f <- get_pgfile(source_name = "ETH ICR GeoEPR",
                  source_version = "2023",
                  id = "3900b527-a728-4c26-b0ab-f4441d3ee2e8")

  df <- sf::st_read(f)

  df <- df |>
    dplyr::mutate(gwsdate = as.Date(paste0(from, "-01-01")),
           gwedate = as.Date(paste0(to, "-12-31"))) |>
    dplyr::mutate(date_interval = lubridate::interval(gwsdate, gwedate))

  return(df)
}
