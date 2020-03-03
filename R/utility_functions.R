#' raster_to_table
#'
#' Converts a raster to a tibble with x, y, mydate and raster value
#' @return A tibble with columns x, y, mydate, and value
raster_to_tibble <- function(rast, add_pg_index = FALSE){
  df <- raster::rasterToPoints(rast)
  df <- dplyr::as_tibble(df)

  if(add_pg_index){
    pg <- priogrid::prio_blank_grid()

    assertthat::assert_that(assertthat::are_equal(extent(rast), extent(pg)))

    pg_df <- raster::rasterToPoints(pg)
    pg_df <- dplyr::as_tibble(pg_df)
    df <- dplyr::left_join(df, pg_df, by = c("x", "y"))

  }

  return(df)
}


# Previous prio_aggregate_raster()
# Obsoletes prio_raster()
# use bilinear resampling for numeric data
raster_to_pg <- function(rast, aggregation_function = "mean", resampling_method = ""){
  resolution_factor <- priogrid::prio_resolution() / raster::res(rast) #previous prio_resolution_factor()

  aggregation_needed <- any(resolution_factor > 1)
  disaggregation_needed <- any(resolution_factor < 1)
  nothing_needed <- all(resolution_factor == 1)
  assertthat::assert_that( xor(xor(aggregation_needed, disaggregation_needed), nothing_needed) )

  if(aggregation_needed){
    rast <- raster::aggregate(rast, fact = resolution_factor, fun = aggregation_function)
  }
  if(disaggregation_needed){
    rast <- raster::disaggregate(rast, fact = 1 / resolution_factor, method = resampling_method)
  }

  raster::crs(rast) <- priogrid::prio_crs()
  raster::extent(rast) <- priogrid::prio_extent()
  return(rast)
}

vector_to_pg <- function(sfdf, variable, need_aggregation = FALSE, missval = -1, fun = fun){
  pg <- priogrid::prio_blank_grid()

  if(!need_aggregation){
    vx <- velox::velox(pg)
    vx$rasterize(spdf = sfdf, field = variable, background = missval, small = TRUE)

    rast <- vx$as.RasterLayer(band = 1)
    rast[rast == missval] <- NA
    return(rast)
  }
  # backup solution when rasterization needs to aggregate values over many polygons/points
  rast <- raster::rasterize(sfdf, priogrid::prio_blank_grid(), field = variable, fun = fun)
  names(rast) <- variable
  crs(rast) <- sf::st_crs(pg)$proj4string
  return(rast)
}

panel_to_pg <- function(df, timevar, variable, need_aggregation, missval, fun){
  time_fact <- factor(df[[timevar]])

  sdf <- dplyr::select(df, !!variable)
  sdf_list <- base::split(sdf, time_fact, sep = "_")
  rast_list <- parallel::mclapply(sdf_list, vector_to_pg, variable = variable, need_aggregation = need_aggregation, missval = missval, fun = fun)

  pg_tibble <- parallel::mclapply(rast_list, raster_to_tibble, add_pg_index = TRUE)

  add_timevar <- function(df, time, timevar){
    df[[timevar]] <- time
    return(df)
  }

  pg_tibble <- purrr::map2_dfr(pg_tibble, names(pg_tibble), add_timevar, timevar = timevar)
  return(pg_tibble)
}

# previous get_array
get_ncarray <- function(ncfile, variable, fillvalue, lon=NULL, lat=NULL, ...){
  nc <- nc_open(ncfile)

  if(missing(lon) & missing(lat)){
    lon <- NA
    lat <- NA
    res <- NA
  } else{
    lon <- ncvar_get(nc, lon, verbose = F)
    lat <- ncvar_get(nc, lat, verbose = F)
    res <- (lon[2]-lon[1])/2

  } # If I want to add spatial indexing to the function, this information is necessary.

  fillvalue <- ncatt_get(nc, varid=variable, attname=fillvalue)

  nc_array <- ncvar_get(nc, variable, ...)
  nc_array[nc_array == fillvalue$value] <- NA
  nc_close(nc)

  return(list("data" = nc_array, "lon" = lon, "lat" = lat, "res" = res))
}

make_raster <- function(nclist, flip_poles=FALSE, transpose=FALSE, crs=NULL){
  if(transpose){
    nclist$data <- t(nclist$data)
    }

  rast <- raster(nclist$data,
                 xmn =  min(nclist$lon)-nclist$res,
                 xmx = max(nclist$lon)+nclist$res,
                 ymn = min(nclist$lat)-nclist$res,
                 ymx = max(nclist$lat)+nclist$res,
                 crs=crs)

  if(flip_poles){
    rast <- flip(rast, direction="y")
    }

  return(rast)
}
