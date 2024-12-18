
## Geocoded Peacekeeping Operations (Geo-PKO)

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

gen_geopko <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()
  # create date column combining year and month columns
  # number of peacekeeping operations?
  r <- rasterize(f, pg) # this creates a dummy on peacekeeping at all or not - must be better!
}

