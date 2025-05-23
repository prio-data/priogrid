read_ucdp_ged <- function(){
  f <- get_pgfile(source_name = "UCDP GED",
                  source_version = "24.1",
                  id = "2e5c66d2-d4e6-4282-9039-5b232b861093")

  unzip(f, exdir = dirname(f))
  df <- readRDS(file.path(dirname(f), unzip(f, list = TRUE)$Name)) |>
    sf::st_as_sf(crs = 4326, coords = c("longitude", "latitude")) |>
    dplyr::mutate(date_interval = lubridate::interval(date_start, date_end))
  #plot(sf::st_geometry(df), pch = ".")

  return(df)
}

ucdpged_distance_within_country <- function(measurement_date, cshp = read_cshapes()){
  cshp <- cshp |> dplyr::filter(measurement_date %within% date_interval)

  pg_interval <- pg_date_intervals()[measurement_date %within% pg_date_intervals()]

  pg <- prio_blank_grid()
  ged <- read_ucdp_ged() |>
    sf::st_transform(crs = sf::st_crs(pg)) |>
    dplyr::filter(lubridate::int_overlaps(date_interval, pg_interval))

  rgw <- cshapes_gwcode(measurement_date, cshp = cshp)
  gwcodes <- unique(cshp$gwcode)

  cover <- cshapes_cover(measurement_date, cshp = cshp)
  values(cover) <- dplyr::if_else(values(cover) == T, 1, NA)

  result <- pg
  result[values(result)] <- 0
  result <- result*cover
  sf::sf_use_s2(FALSE)
  distances <- list()
  for(gwcode in gwcodes){
    tmp <- rgw
    tmp[values(tmp) != gwcode] <- NA
    tmp[!is.na(values(tmp))] <- 1

    ged_sub <- ged[sf::st_intersects(ged, cshp[cshp$gwcode == gwcode, ], sparse = FALSE),]
    if(nrow(ged_sub) > 0){
      dist <- terra::distance(tmp, terra::vect(ged_sub), rasterize = T)
      dist <- (dist*tmp)
      dist[is.na(dist)] <- 0
      result <- result + dist
    }
  }
  sf::sf_use_s2(TRUE)

  #plot(1/log1p(result))
  #plot(1/result)
  return(result)
}

gen_ucdpged_distance_within_country <- function(ged = read_ucdp_ged(), cshp = read_cshapes()){
  time_slices <- pg_dates()
  temporal_interval <- lubridate::interval(min(ged$date_start), max(ged$date_end))
  time_slices <- time_slices[time_slices %within% temporal_interval]

  # Extrapolate cShapes to last UCDP GED date.
  cshp <- cshp |> dplyr::mutate(gweyear = dplyr::if_else(gweyear == max(gweyear), lubridate::year(max(ged$date_end)), gweyear)) |>
    dplyr::mutate(gwedate = as.Date(paste(gweyear, gwemonth, gweday, sep = "-")))

  r <- ucdpged_distance_within_country(time_slices[1], cshp = cshp)
  for(i in 2:length(time_slices)){
    t <- time_slices[i]
    terra::add(r) <- ucdpged_distance_within_country(t, cshp = cshp)
  }
  names(r) <- as.character(time_slices)
  r
}
