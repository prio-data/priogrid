

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


gen_geoEPR <- function(df = read_geoEPR(), pg = prio_blank_grid(), date_interval = NULL) {

  if (!is.null(date_interval)) {
    geoEPR_sf <- df |>
      dplyr::filter(date_interval == date_interval) |>
      dplyr::filter(!sf::st_is_empty(geometry))
  }

  raster_layers <- list()

  for (grp in unique(geoEPR_sf$group)) {
    group_sf <- geoEPR_sf |>
      dplyr::filter(group == grp)

    exact_values <- exactextractr::exact_extract(x = pg, y = group_sf, fun = "mode", progress = FALSE)

    layer_raster <- terra::rast(pg)
    layer_raster[] <- NA
    layer_raster[] <- unlist(lapply(exact_values, function(x) if (is.na(x)) NA else grp))

    raster_layers[[as.character(grp)]] <- layer_raster
  }

  raster_stack <- do.call(c, raster_layers)

  return(raster_stack)
}


