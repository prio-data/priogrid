

read_geoEPR <- function() {
  f <- get_pgfile(source_name = "ETH ICR GeoEPR",
                  source_version = "2021",
                  id = "3900b527-a728-4c26-b0ab-f4441d3ee2e8")

  df <- sf::st_read(f)
}
