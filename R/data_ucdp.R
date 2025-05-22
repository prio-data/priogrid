read_ucdp <- function(){
  f <- get_pgfile(source_name = "UCDP GED",
                  source_version = "24.1",
                  id = "2e5c66d2-d4e6-4282-9039-5b232b861093")

  unzip(f, exdir = dirname(f))
  df <- readRDS(file.path(dirname(f), unzip(f, list = TRUE)$Name)) |>
    sf::st_as_sf(crs = 4326, coords = c("longitude", "latitude"))
  #plot(sf::st_geometry(df), pch = ".")

  return(df)
}

