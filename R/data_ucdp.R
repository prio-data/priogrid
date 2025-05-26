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
    dplyr::filter(lubridate::int_overlaps(date_interval, pg_interval)) |>
    dplyr::filter(where_prec < 6) # Remove where_prec >= 6 (i.e. not sub-national precision)

  rgw <- cshapes_gwcode(measurement_date, cshp = cshp)
  gwcodes <- unique(cshp$gwcode)

  cover <- cshapes_cover(measurement_date, cshp = cshp)
  values(cover) <- dplyr::if_else(values(cover) == T, 1, NA)

  result <- pg
  result[values(result)] <- 0
  result <- result*cover
  suppressMessages(sf::sf_use_s2(FALSE)) # Only use sf here to subset data.
  distances <- list()
  for(gwcode in gwcodes){
    tmp <- rgw
    tmp[values(tmp) != gwcode] <- NA
    tmp[!is.na(values(tmp))] <- 1

    suppressMessages(
      ged_sub <- ged[sf::st_intersects(ged, cshp[cshp$gwcode == gwcode, ], sparse = FALSE),]
    )

    if(nrow(ged_sub) > 0){
      dist <- terra::distance(tmp, terra::vect(ged_sub), rasterize = T)
      dist <- (dist*tmp)
      dist[is.na(dist)] <- 0
      result <- result + dist
    } else{
      # If no GED events in country, then set the distance to missing (to separate from being 0 m from events)
      tmp[is.na(values(tmp))] <- 0
      tmp[values(tmp) == 1] <- NA
      tmp[values(tmp) == 0] <- 1
      result <- result * tmp
    }
  }
  suppressMessages(sf::sf_use_s2(TRUE))

  #plot(1/log1p(result))
  #plot(1/result)
  return(result)
}

#' Generate UCDP GED distance within country
#'
#' Calculates UCDP GED distance to the nearest UCDP GED even within each country for each time interval in [pg_date_intervals()].
#'
#' @param ged UCDP GED sf data frame, defaults to [read_ucdp_ged()]
#' @param cshp cShapes sf data frame, defaults to [read_cshapes()]
#'
#' @returns SpatRaster with multiple layers, one for each time interval in [pg_date_intervals()].
#' @export
gen_ucdpged_distance_within_country <- function(ged = read_ucdp_ged(), cshp = read_cshapes()){
  time_slices <- pg_dates()
  temporal_interval <- lubridate::interval(min(ged$date_start), max(ged$date_end))
  time_slices <- time_slices[time_slices %within% temporal_interval]

  # Extrapolate cShapes to last UCDP GED date.
  cshp <- cshp |> dplyr::mutate(gweyear = dplyr::if_else(gweyear == max(gweyear), lubridate::year(max(ged$date_end)), gweyear)) |>
    dplyr::mutate(gwedate = as.Date(paste(gweyear, gwemonth, gweday, sep = "-"))) |>
    dplyr::mutate(date_interval = lubridate::interval(gwsdate, gwedate))


  # This does speed up things, but will need to debug.
  # if(ncores > 1){
  #   doParallel::registerDoParallel(ncores)
  #   raster_list <- foreach::foreach(i = 1:length(time_slices)) %dopar% {
  #     t <- time_slices[i]
  #     rt <- ucdpged_distance_within_country(t, cshp = cshp)
  #     names(rt) <- as.character(t)
  #     return(rt)
  #   }
  #   doParallel::stopImplicitCluster()
  # } else{
  #   raster_list <- foreach::foreach(i = 1:length(time_slices)) %do% {
  #     t <- time_slices[i]
  #     print(t)
  #     rt <- ucdpged_distance_within_country(t, cshp = cshp)
  #     names(rt) <- as.character(t)
  #     return(rt)
  #   }
  # }
  #plot(1/((raster_list[[1]]/1e6)+1))
  r <- ucdpged_distance_within_country(time_slices[1], cshp = cshp)
  for(i in 2:length(time_slices)){
    t <- time_slices[i]
    print(paste("Processing", i,"/",  length(time_slices)))
    rt <- ucdpged_distance_within_country(t, cshp = cshp)
    names(rt) <- as.character(t)
    terra::add(r) <- rt
  }
  names(r) <- as.character(time_slices)
  r
}
