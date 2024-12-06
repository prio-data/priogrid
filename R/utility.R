#' Create matrix with index numbering conventions as for PRIO-GRID.
#'
#' @param ncol The number of columns in the grid.
#' @param nrow The number of rows in the grid.
#' @return A ncol*nrow matrix with integer indices.
#' @examples
#' pg <- create_pg_indices()
#' @export
create_pg_indices <- function(ncol = pgoptions$get_ncol(),
                              nrow = pgoptions$get_nrow()){
  # To create PRIO-GRID, swap ncol and nrow, load index in reverse order, and
  # rotate 90 degrees once.
  rotate <- function(x) t(apply(x, 2, rev))
  pg <- rotate(matrix(rev(1:(ncol*nrow)), nrow=ncol, ncol=nrow))

  return(pg)
}


#' Create a raster with PRIO-GRID ids.
#'
#' @param ncol Number of columns
#' @param nrow Number of rows
#' @param extent The extent of the raster as a vector c(xmin, xmax, ymin, ymax)
#' @param crs_string A CRS-string. Description of the Coordinate Reference System (map projection) in PROJ.4, WKT or authority:code notation.
#' @return SpatRaster
#' @examples
#' pg <- prio_blank_grid()
#' @export
prio_blank_grid <- function(ncol = pgoptions$get_ncol(),
                            nrow = pgoptions$get_nrow(),
                            extent = pgoptions$get_extent(),
                            crs_string = pgoptions$get_crs()){
  require(terra)

  if(ncol%%1 != 0 & ncol > 0) stop("ncol must be positive integer")
  if(nrow%%1 != 0 & nrow > 0) stop("nrow must be positive integer")

  pg <- rast(ext(extent), crs = crs_string, ncol = ncol, nrow = nrow)
  values(pg) <- create_pg_indices(ncol, nrow)
  names(pg) <- "pgid"
  return(pg)
}

#' Get a sequence of dates
#'
#' This is a wrapper of [base::seq.Date], only with defaults that can be set in the options.
#'
#'
#' @param from starting date
#' @param to end date
#' @param by increment of sequence. See details in [base::seq.Date].
#'
#' @return Date vector
#' @export
#'
#' @examples
#' pg_dates()
pg_dates <- function(start_date = pgoptions$get_start_date(),
                     end_date = pgoptions$get_end_date(),
                     temporal_resolution = pgoptions$get_temporal_resolution()){
  seq.Date(start_date, end_date, temporal_resolution)
}

#' Converts raster with variable to data.frame
#'
#' Assumes that the name of the raster layer is the name of the variable if static is true,
#' otherwise, the user must supply the correct variable name. If static is false,
#' the name of the raster layer is assumed to be the time variable.
#'
#' @param rast SpatRaster
#' @param static True if no temporal dimension, False else.
#' @param varname The variable name, only required if static is False.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' ne <- gen_naturalearth_cover()
#' rast_to_df(ne)
rast_to_df <- function(rast, static = TRUE, varname = NULL){
  pg <- prio_blank_grid()
  df <- c(pg, rast) |> as.data.frame()

  if(static){
    return(df)
  } else{
    # Assumes variable names in raster are dates.
    df <- df |> tidyr::pivot_longer(cols = -dplyr::all_of(c("pgid")), names_to = "measurement_date", values_to = varname)
    return(df)
  }
}

#' Transform raster to PRIO-GRID format
#'
#' This should generally not be used. Write exactly what you need to transform the data instead.
#' It can be used as a general template for how to transform data, however.
#'
#' @param r Raster to transform
#' @param agg_fun Aggregation function, see terra::aggregate
#' @param disagg_method Disaggregation method, see terra::disagg
#' @param cores The number of cores to use when aggregating data
#' @return SpatRaster
#' @examples
#' r <- prio_blank_grid(ncol = 1440, nrow = 720)
#' r <- project(r, "ESRI:54009")
#' robust_transformation(r, agg_fun = "mean")
#' @export
robust_transformation <- function(r, agg_fun, disagg_method = "near", cores = 1, ...){
  require(terra)

  pg <- prio_blank_grid()

  equal_projection <- crs(r) == crs(pg)
  if(!equal_projection){
    r <- project(r, crs(pg))
  }

  equal_extent <- ext(r) == ext(pg)
  if(!equal_extent){
    tmp <- rast(ext(pg),
               crs = crs(r),
               ncol = ncol(r),
               nrow = nrow(r))
    r <- resample(r, tmp, method = "near", threads = T)
  }

  higher_resolution <- res(r) < res(pg)
  if(any(higher_resolution)){
    r <- aggregate(r,
                  fact = res(pg)/res(r),
                  fun = agg_fun,
                  cores = cores, ...)
  }

  lower_resolution <- res(r) > res(pg)
  if(any(lower_resolution)){
    r <- disagg(r,
               fact = res(r)/res(pg),
               method = disagg_method)
  }

  r <- resample(r, pg, method = "near", threads = T)

  return(r)
}

#' Convert a pgraster to tabular format
#'
#' @param r An input raster that must have the same projection, resolution, and extent as PRIO-GRID
#' @return tibble
#' @examples
#' pg <- prio_blank_grid()
#' names(pg) <- "test"
#' raster_to_pgtibble(pg)
#' @export
raster_to_pgtibble <- function(r){
  require(terra)
  require(assertthat)
  require(dplyr)

  pg <- prio_blank_grid()
  assert_that(all(res(r) == res(pg)))
  assert_that(ext(r) == ext(pg))
  assert_that(crs(r) == crs(pg))

  df <- as.data.frame(c(pg, r)) |> as_tibble()
  return(df)
}
