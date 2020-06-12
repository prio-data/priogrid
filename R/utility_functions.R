#' raster_to_table
#'
#' Converts a raster to a tibble with x, y, mydate and raster value
#'
#' @param rast a raster object
#' @param add_pg_index boolean, whether to add a pgid column or not.
#'
#' @return A tibble with columns x, y, and value.
#' @export
raster_to_tibble <- function(rast, add_pg_index = FALSE){
  df <- raster::rasterToPoints(rast)
  df <- dplyr::as_tibble(df)

  if(add_pg_index){
    pg <- priogrid::prio_blank_grid()

    assertthat::assert_that(assertthat::are_equal(raster::extent(rast), raster::extent(pg)))

    pg_df <- raster::rasterToPoints(pg)
    pg_df <- dplyr::as_tibble(pg_df)
    df <- dplyr::left_join(df, pg_df, by = c("x", "y"))

  }

  return(df)
}

#' get_closest_distance
#'
#' Finds (element-wise) the closest distance between a set of points and a set of features.
#'
#' @param points point data of class sfg, sfc, or sf
#' @param features object of class sfg, sfc, or sf
#' @param check_dateline boolean, whether or not to account for the possibility that the shortest distance is crossing the dateline. doubles the time it takes.
#'
#' @return A tibble with columns x, y, and value.
#' @export
get_closest_distance <- function(points, features, check_dateline=TRUE){
  nearest_feature <- sf::st_nearest_feature(points, features)
  nearest_point <- sf::st_nearest_points(points, features[nearest_feature,], pairwise = TRUE)

  distances <- sf::st_length(nearest_point)

  if(check_dateline){
    pointsT <- st_transform(points,"+proj=longlat +datum=WGS84 +pm=180") %>% st_wrap_dateline()
    featuresT <- st_transform(features,"+proj=longlat +datum=WGS84 +pm=180") %>% st_wrap_dateline()
    nearest_featureT <- sf::st_nearest_feature(pointsT, featuresT)
    nearest_pointT <- sf::st_nearest_points(pointsT, featuresT[nearest_featureT,], pairwise = TRUE)
    distancesT <- sf::st_length(nearest_pointT)
    distances <- apply(cbind(distances, distancesT), 1, min)
  }
  return(distances)
}

update_cells_iteratively <- function(pg_list, varname, changed_areas){
  # Update classification scheme iteratively. pg_list only gives information of the cells that have changed.
  pg <- priogrid::prio_blank_grid()

  rasters <- list()
  i <- 1
  current_raster <- pg
  current_raster[] <- NA
  for(j in 1:length(pg_list)){
    crossection <- pg_list[[j]]
    current_raster[match(crossection$pgid, pg[])] <- crossection[[varname]]
    rasters[[i]] <- current_raster
    i <- i + 1
  }

  pg_raster <- raster::stack(rasters)

  crossection_dates <- sapply(changed_areas, function(x) unique(x$crossection_date))
  crossection_dates <- as.Date(crossection_dates, origin = as.Date("1970-1-1"))

  result <- dplyr::tibble()
  for(j in 1:dim(pg_raster)[3]){
    rast <- raster::subset(pg_raster, j)
    df <- raster_to_tibble(rast)
    names(df)[3] <- varname
    df$year <- lubridate::year(crossection_dates)[j]
    df$month <- lubridate::month(crossection_dates)[j]
    df$day <- lubridate::day(crossection_dates)[j]
    result <- dplyr::bind_rows(result, df)
  }

  return(result)
}

#' raster_to_pg
#'
#' Aggregates or disaggregates a raster to conform with the PRIO-GRID resolution.
#'
#'
#' @param rast a raster object
#' @param aggregation_function see raster::aggregate fun
#' @param resampling_method see raster::disaggregate, default is ''
#'
#' @return A raster with the same resolution, crs, and extent as PRIO-GRID.
#' @export
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

  if(raster::extent(rast) != priogrid::prio_extent()){
    rast <- priogrid::rasterextent_to_pg(rast)
  }

  raster::crs(rast) <- priogrid::prio_crs()
  return(rast)
}


#' rasterextent_to_pg
#'
#' Resamples a raster using the nearest neighbor method to make extent the same as PRIO-GRID.
#'
#'
#' @param rast a raster object
#'
#' @return A raster with same extent and crs as PRIO-GRID.
#' @export
rasterextent_to_pg <- function(rast){
  rast <- raster::resample(rast, priogrid::prio_blank_grid(), method = "ngb")
  return(rast)
}


#' vector_to_pg
#'
#' Converts a sf data frame to PRIO-GRID as a crossection.
#'
#'
#' @param sfdf a sf (simple features) data frame
#' @param variable the variable to convert to PRIO-GRID
#' @param fun aggregation function. only used if need_aggregation is TRUE.
#' @param need_aggregation if FALSE, will use velox, which only extracts the last value for each feature. fast when applicable.
#' @param missval only used if need_aggregation is FALSE. velox sets missing data to this value.
#'
#' @return A raster with same extent and crs as PRIO-GRID.
#' @export
vector_to_pg <- function(sfdf, variable, fun, need_aggregation = TRUE, missval = -1){
  pg <- priogrid::prio_blank_grid()

  # if(!need_aggregation){
  #   vx <- velox::velox(pg)
  #   vx$rasterize(spdf = sfdf, field = variable, background = missval, small = TRUE)
  #
  #   rast <- vx$as.RasterLayer(band = 1)
  #   rast[rast == missval] <- NA
  #   names(rast) <- variable
  #   return(rast)
  # }
  # backup solution when rasterization needs to aggregate values over many polygons/points
  rast <- raster::rasterize(sfdf, priogrid::prio_blank_grid(), field = variable, fun = fun)
  names(rast) <- variable
  raster::crs(rast) <- sf::st_crs(pg)$proj4string
  return(rast)
}

#' panel_to_pg
#'
#' Converts a sf data frame with panel data to PRIO-GRID.
#'
#'
#' @param df a sf (simple features) data frame with panel data
#' @param timevar the temporal variable (used to split data into crossections)
#' @param variable the variable to convert to PRIO-GRID
#' @param need_aggregation if FALSE, will use velox, which only extracts the last value for each feature. fast when applicable.
#' @param missval only used if need_aggregation is FALSE. velox sets missing data to this value.
#' @param fun aggregation function. only used if need_aggregation is TRUE.
#'
#' @return A tibble with x, y, pgid, timevar, and variable.
#' @export
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


#' interpolate_crossection
#'
#' Increasingly coarse bilinear interpolation of missing data
#'
#' @param crossection a dataframe crossection lon, lat, variable
#' @param variable the variable to interpolate missing from
#' @param lon a string denoting the name of the longitude variable
#' @param lat a string denoting the name of the latitude variable
#'
#' @return the interpolated crossection
#' @export
interpolate_crossection <- function(crossection, variable, lon, lat, input_folder, date_var = NULL){
  if(!is.null(date_var)){
    crossection_date <- unique(crossection[[date_var]])
  }


  rast <- raster::rasterFromXYZ(dplyr::select(crossection, all_of(c(lon, lat, variable))))

  rast2 <- raster::resample(rast, priogrid::prio_blank_grid(ncol = 360, nrow = 180), method = "bilinear")
  rast3 <- raster::resample(rast, priogrid::prio_blank_grid(ncol = 180, nrow = 90), method = "bilinear")
  rast4 <- raster::resample(rast, priogrid::prio_blank_grid(ncol = 90, nrow = 45), method = "bilinear")
  rast5 <- raster::resample(rast, priogrid::prio_blank_grid(ncol = 45, nrow = 23), method = "bilinear")
  rast6 <- raster::resample(rast, priogrid::prio_blank_grid(ncol = 23, nrow = 11), method = "bilinear")
  rast7 <- raster::resample(rast, priogrid::prio_blank_grid(ncol = 11, nrow = 6), method = "bilinear")

  rast2 <- raster::resample(rast2, priogrid::prio_blank_grid(), method = "ngb")
  rast3 <- raster::resample(rast3, priogrid::prio_blank_grid(), method = "ngb")
  rast4 <- raster::resample(rast4, priogrid::prio_blank_grid(), method = "ngb")
  rast5 <- raster::resample(rast5, priogrid::prio_blank_grid(), method = "ngb")
  rast6 <- raster::resample(rast6, priogrid::prio_blank_grid(), method = "ngb")
  rast7 <- raster::resample(rast7, priogrid::prio_blank_grid(), method = "ngb")

  missing_vals <- which(is.na(rast[]))
  rast[missing_vals] <- rast2[missing_vals]
  missing_vals <- which(is.na(rast[]))
  rast[missing_vals] <- rast3[missing_vals]
  missing_vals <- which(is.na(rast[]))
  rast[missing_vals] <- rast4[missing_vals]
  missing_vals <- which(is.na(rast[]))
  rast[missing_vals] <- rast5[missing_vals]
  missing_vals <- which(is.na(rast[]))
  rast[missing_vals] <- rast6[missing_vals]
  missing_vals <- which(is.na(rast[]))
  rast[missing_vals] <- rast7[missing_vals]

  pgland <- file.path(input_folder, "cshapes", "cache", "pgland.parquet")
  assertthat::assert_that(file.exists(pgland))
  pgland <- arrow::read_parquet(pgland)


  pg <- priogrid::prio_blank_grid()
  rast[which(!(pg[] %in% pgland$pgid))] <- NA

  sdf <- priogrid::raster_to_tibble(rast)
  if(!is.null(date_var)){
    sdf[[datevar]] <-crossection_date
  }

  sdf
}


#' missing_in_pg
#'
#' Finds whether a crossection has missing data for any of the 64818 PRIO-GRID land cells,
#' and plots them.
#'
#' @param df a dataframe crossection x, y, variable
#' @param variable the variable to convert to plot
#' @param ... function accepts other parameters passed to raster::plot()
#'
#' @return A dataframe with the cells that are missing and a plot.
#' @export
missing_in_pg <- function(df, plot_missing = TRUE, ...){
  pgland <- file.path(input_folder, "cshapes", "cache", "pgland.parquet")
  assertthat::assert_that(file.exists(pgland))
  pgland <- arrow::read_parquet(pgland)

  pg <- prio_blank_grid()
  pgdf <- raster_to_tibble(pg)

  anti_df <- anti_join(pgdf, df, by = c("x", "y"))

  anti_df <- dplyr::filter(anti_df, pgid %in% pgland$pgid)

  if(plot_missing){
    rast <- rasterFromXYZ(anti_df)
    raster::plot(rast, ...)
  }
  return(anti_df)
}


#' map_pg_crossection
#'
#' Plots a PRIO-GRID crossection on a map.
#'
#'
#' @param pgdf a prio-grid dataframe with x, y, year, month, variable
#' @param variable the variable to convert to plot
#' @param myyear the year of the crossection to plot
#' @param mymonth the month of the crossection to plot
#' @param ... function accepts other parameters passed to raster::plot()
#'
#' @return A plot based on raster::plot()
#' @export
map_pg_crossection <- function(pgdf, variable, myyear = NULL, mymonth = NULL, ...){
  variable <- dplyr::enquo(variable)
  if(!is.null(mymonth)){
    cs <- dplyr::filter(pgdf, year == myyear, month == mymonth) %>% dplyr::select(x, y, !!variable)
  } else if(!is.null(myyear)){
    cs <- dplyr::filter(pgdf, year == myyear) %>% dplyr::select(x, y, !!variable)
  } else {
    cs <- dplyr::select(pgdf, x, y, !!variable)
  }

  rast <- raster::rasterFromXYZ(cs)
  rast <- priogrid::rasterextent_to_pg(rast)
  rast <- priogrid::raster_to_pg(rast)

  raster::plot(rast, ...)
}

# previous get_array
get_ncarray <- function(ncfile, variable, fillvalue, lon=NULL, lat=NULL, ...){
  nc <- ncdf4::nc_open(ncfile)

  if(missing(lon) & missing(lat)){
    lon <- NA
    lat <- NA
    res <- NA
  } else{
    lon <- ncdf4::ncvar_get(nc, lon, verbose = F)
    lat <- ncdf4::ncvar_get(nc, lat, verbose = F)
    res <- (lon[2]-lon[1])/2

  } # If I want to add spatial indexing to the function, this information is necessary.

  fillvalue <- ncdf4::ncatt_get(nc, varid=variable, attname=fillvalue)

  nc_array <- ncdf4::ncvar_get(nc, variable, ...)
  nc_array[nc_array == fillvalue$value] <- NA
  ncdf4::nc_close(nc)

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
