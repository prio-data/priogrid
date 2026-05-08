#' Create matrix with index numbering conventions as for PRIO-GRID.
#'
#' @param config A `pg_config` object. Defaults to [pg_current_config()].
#' @return A ncol*nrow matrix with integer indices.
#' @examples
#' pg <- create_pg_indices()
#' @export
create_pg_indices <- function(config = pg_current_config()){
  ncol <- config$ncol
  nrow <- config$nrow
  # To create PRIO-GRID, swap ncol and nrow, load index in reverse order, and
  # rotate 90 degrees once.
  rotate <- function(x) t(apply(x, 2, rev))
  pg <- rotate(matrix(rev(1:(ncol*nrow)), nrow=ncol, ncol=nrow))

  return(pg)
}


#' Create a raster with PRIO-GRID ids.
#'
#' @param config A `pg_config` object. Defaults to [pg_current_config()].
#' @return SpatRaster
#' @examples
#' \dontrun{
#' pg <- prio_blank_grid()
#' }
#' @export
prio_blank_grid <- function(config = pg_current_config()){
  rlang::check_installed("terra", reason = "to construct a PRIO-GRID SpatRaster")

  ncol <- config$ncol
  nrow <- config$nrow
  extent <- config$extent
  crs_string <- config$crs

  if(ncol%%1 != 0 & ncol > 0) stop("ncol must be positive integer")
  if(nrow%%1 != 0 & nrow > 0) stop("nrow must be positive integer")

  pg_lonlat <- terra::rast(terra::ext(extent), crs = "epsg:4326", ncol = ncol, nrow = nrow)
  terra::values(pg_lonlat) <- create_pg_indices(config)
  pg_lonlat <- terra::project(pg_lonlat, crs_string)

  pg <- terra::deepcopy(pg_lonlat)
  terra::values(pg) <- create_pg_indices(pg_config(ncol = terra::ncol(pg), nrow = terra::nrow(pg)))
  pg <- terra::ifel(is.nan(pg_lonlat), NaN, pg)



  names(pg) <- "pgid"
  return(pg)
}

#' Get a sequence of dates
#'
#' This is a wrapper of [base::seq.Date], only with defaults that can be set in the config.
#'
#' @param config A `pg_config` object. Defaults to [pg_current_config()].
#'
#' @return Date vector
#' @export
#'
#' @examples
#' pg_dates()
pg_dates <- function(config = pg_current_config()){
  start_date <- config$start_date
  end_date <- config$end_date
  temporal_resolution <- config$temporal_resolution

  unit <- dplyr::case_when(
    grepl("month", temporal_resolution) ~ "month",
    grepl("week", temporal_resolution) ~ "week",
    grepl("quarter", temporal_resolution) ~ "quarter",
    grepl("year", temporal_resolution) ~ "year"
  )

  is_end_of_month <- start_date == (lubridate::ceiling_date(start_date, "month") - lubridate::days(1))

  if(is_end_of_month & unit != "year"){
    base_seq <- seq.Date(lubridate::floor_date(start_date, unit), end_date, temporal_resolution)
    base_seq <- lubridate::ceiling_date(base_seq, "month") - lubridate::days(1)
  } else{
    base_seq <- seq.Date(start_date, end_date, temporal_resolution)
  }

  base_seq <- base_seq[base_seq <= end_date]
  base_seq[base_seq >= start_date]
}

#' Get a sequence of date intervals
#'
#' The start date is the last observed date in the first interval of the intervals you
#' want to capture. E.g., if start date is 31 January 2010 with 1 month resolution, then the
#' first interval goes from 1 January 2010 to 31 January 2010. The intervals are not necessary
#' equal length.
#'
#' @param config A `pg_config` object. Defaults to [pg_current_config()].
#'
#' @return Date interval vector
#' @export
#'
#' @examples
#' pg_date_intervals()
pg_date_intervals <- function(config = pg_current_config()){
  start_date <- config$start_date
  end_date <- config$end_date
  temporal_resolution <- config$temporal_resolution

  unit <- dplyr::case_when(
    grepl("month", temporal_resolution) ~ "month",
    grepl("week", temporal_resolution) ~ "week",
    grepl("quarter", temporal_resolution) ~ "quarter",
    grepl("year", temporal_resolution) ~ "year"
  )

  is_end_of_month <- start_date == (lubridate::ceiling_date(start_date, "month") - lubridate::days(1))

  base_seq <- pg_dates(config)
  floor <- lubridate::floor_date(base_seq, unit)

  if(is_end_of_month & unit == "month"){
    previous <- seq.Date(lubridate::floor_date(start_date, unit), end_date, temporal_resolution)
  } else{
    previous <- seq.Date(base_seq[1], length.out = 2, by = paste("-1", unit))[2] + lubridate::days(1)
    previous <- seq.Date(previous, length.out = length(base_seq), by = paste("1", unit))
  }

  lubridate::interval(previous[previous < max(base_seq)], base_seq)
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
#' @param config A `pg_config` object. Defaults to [pg_current_config()].
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ne <- gen_naturalearth_cover()
#' rast_to_df(ne)
#' }
rast_to_df <- function(rast, static = TRUE, varname = NULL, config = pg_current_config()){
  pg <- prio_blank_grid(config)
  df <- c(pg, rast) |> as.data.frame()
  df <- df[rowSums(!is.na(df)) > 1,] # remove rows with only missing elements.

  if(static){
    df <- data.table::setDT(df, key = "pgid")
    return(df)
  } else{
    # Assumes variable names in raster are dates.
    df <- tidyr::pivot_longer(df,
                              cols = -dplyr::all_of(c("pgid")),
                              names_to = "measurement_date",
                              values_to = varname,
                              names_transform = as.Date)
    df <- data.table::setDT(df, key = c("pgid", "measurement_date"))
    return(df)
  }
}

#' Robustly transform any raster to PRIO-GRID format
#'
#' @description
#' This function performs a comprehensive transformation of input rasters to match
#' the PRIO-GRID specification. It handles rasters with different projections,
#' extents, and resolutions through an intelligent workflow that includes
#' reprojection, cropping, aggregation, disaggregation, and final resampling
#' as needed.
#'
#' @details
#' The transformation workflow automatically detects and handles:
#' \itemize{
#'   \item **Projection differences**: Reprojects to PRIO-GRID CRS if needed
#'   \item **Extent mismatches**: Crops input if larger than PRIO-GRID extent
#'   \item **Resolution differences**:
#'     \itemize{
#'       \item Aggregates high-resolution data using specified aggregation function
#'       \item Disaggregates low-resolution data using specified method
#'     }
#'   \item **Final alignment**: Uses nearest-neighbor resampling for exact grid matching
#' }
#'
#' All intermediate files are written to temporary storage to handle large datasets
#' efficiently and are automatically cleaned up after processing.
#'
#' @param r SpatRaster object to transform. Can have any projection, extent, or resolution.
#' @param agg_fun Character string or function for aggregating high-resolution data.
#'   Common options include "mean", "sum", "max", "min", "median", "modal".
#'   See \code{\link[terra]{aggregate}} for all options.
#' @param disagg_method Character string specifying disaggregation method for
#'   low-resolution data. Options are "near" (nearest neighbor, default),
#'   "bilinear", or "cubic". See \code{\link[terra]{disagg}} for details.
#' @param config A `pg_config` object. Defaults to [pg_current_config()].
#' @param cores Integer specifying number of CPU cores to use for aggregation
#'   operations. Defaults to 1. Higher values can speed up processing of large datasets.
#' @param ... Additional arguments passed to \code{\link[terra]{aggregate}}.
#'   Useful for controlling aggregation behavior (e.g., na.rm = TRUE).
#'
#' @return SpatRaster object conforming to PRIO-GRID specifications:
#' \itemize{
#'   \item CRS: As specified in \code{config} (default: EPSG:4326)
#'   \item Extent: As specified in \code{config} (default: global extent)
#'   \item Resolution: Calculated from \code{nrow}/\code{ncol} in \code{config}
#'   \item Grid alignment: Exactly matched to PRIO-GRID cell boundaries
#' }
#'
#' @section Performance Notes:
#' For large datasets, consider:
#' \itemize{
#'   \item Increasing \code{cores} parameter for faster aggregation
#'   \item Ensuring adequate disk space in the raw data folder for temporary files
#'   \item Pre-cropping input data to region of interest before transformation
#' }
#'
#' @examples
#' \dontrun{
#' r <- read_ghsl_population_grid()
#' res <- robust_transformation(r, agg_fun = "sum")
#' }
#'
#' @seealso
#' \code{\link{prio_blank_grid}} for creating empty PRIO-GRID templates,
#' \code{\link[terra]{aggregate}}, \code{\link[terra]{disagg}}, \code{\link[terra]{resample}}
#'
#' @export
robust_transformation <- function(r, agg_fun, disagg_method = "near", config = pg_current_config(), ...){
  rlang::check_installed("terra")
  pg_configure_terra_memory(config)
  pg <- prio_blank_grid(config)
  temporary_directory <- file.path(pg_rawfolder(), "tmp", tempdir() |> basename())
  dir.create(temporary_directory, recursive = TRUE)

  equal_projection <- terra::crs(r) == terra::crs(pg)
  if(!equal_projection){
    tmp1 <- tempfile(pattern = "reprojection", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::project(r, terra::crs(pg), filename = tmp1)
  }

  pg_extent <- terra::vect(terra::ext(pg)) |> sf::st_as_sf()
  input_rast_extent <- terra::vect(terra::ext(r)) |> sf::st_as_sf()
  input_extent_is_larger_or_equal <- sf::st_contains(input_rast_extent, pg_extent, sparse = FALSE) |> all()
  input_extent_is_equal <- terra::ext(pg) == terra::ext(r)
  input_extent_is_larger <- input_extent_is_larger_or_equal & !input_extent_is_equal
  if(input_extent_is_larger){
    tmp2 <- tempfile(pattern = "crop", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::crop(r, pg, filename = tmp2)
  }

  higher_resolution <- terra::res(r) < terra::res(pg)
  if(any(higher_resolution)){
    tmp3 <- tempfile(pattern = "aggregate", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::aggregate(r,
                  fact = terra::res(pg)/terra::res(r),
                  fun = agg_fun,
                  filename = tmp3,
                  ...)
  }

  lower_resolution <- terra::res(r) > terra::res(pg)
  if(any(lower_resolution)){
    tmp4 <- tempfile(pattern = "disaggregate", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::disagg(r,
               fact = terra::res(r)/terra::res(pg),
               method = disagg_method,
               filename = tmp4)
  }

  r <- terra::resample(r, pg, method = "near", threads = T)

  unlink(temporary_directory, recursive = TRUE)
  return(r)
}

#' Add source to CSV file
#'
#' Only use this in devtools environment.
#'
#' @param source Source object to add
#' @param csv_file Path to CSV file
#' @return Invisible NULL
add_source <- function(source, csv_file = "data_raw/sources.csv") {
  if (!inherits(source, "Source")) {
    stop("source must be a Source object")
  }

  # Convert to tibble
  source_tibble <- source$to_tibble()

  # Save URLs to file if necessary
  source$save_url_files()

  # Create or append to CSV
  if (!file.exists(csv_file)) {
    readr::write_delim(source_tibble, csv_file, delim = "\t")
  } else {
    readr::write_delim(source_tibble, csv_file, append = TRUE, delim = "\t")
  }

  invisible(NULL)
}


