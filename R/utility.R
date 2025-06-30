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

#' Get a sequence of date intervals
#'
#' The start date is the last observed date in the first interval of the intervals you
#' want to capture. E.g., if start date is 31 January 2010 with 1 month resolution, then the
#' first interval goes from 1 January 2010 to 31 January 2010. The intervals are not necessary
#' equal length.
#'
#' @param from starting date
#' @param to end date
#' @param by increment of sequence. See details in [base::seq.Date].
#'
#' @return Date interval vector
#' @export
#'
#' @examples
#' pg_date_intervals()
pg_date_intervals <- function(start_date = pgoptions$get_start_date(),
                              end_date = pgoptions$get_end_date(),
                              temporal_resolution = pgoptions$get_temporal_resolution()){
  mydates <- seq.Date(start_date, end_date, temporal_resolution)
  previous_date <- seq.Date(start_date, length.out = 2, by = paste0("-",temporal_resolution))[2] # Get the previous date in the interval
  mydates <- c(previous_date, mydates)
  lubridate::interval(mydates[-length(mydates)] + lubridate::days(1), mydates[-1])
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
  pg <- prio_blank_grid()
  temporary_directory <- file.path(pgoptions$get_rawfolder(), "tmp", tempdir() |> basename())
  dir.create(temporary_directory)

  equal_projection <- terra::crs(r) == terra::crs(pg)
  if(!equal_projection){
    tmp1 <- tempfile(pattern = "reprojection", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::project(r, terra::crs(pg), filename = tmp1, gdal=c("COMPRESS=LZW"))
  }

  pg_extent <- terra::vect(terra::ext(pg)) |> sf::st_as_sf()
  input_rast_extent <- terra::vect(terra::ext(r)) |> sf::st_as_sf()
  input_extent_is_larger_or_equal <- sf::st_contains(input_rast_extent, pg_extent, sparse = FALSE) |> all()
  input_extent_is_equal <- terra::ext(pg) == terra::ext(r)
  input_extent_is_larger <- input_extent_is_larger_or_equal & !input_extent_is_equal
  if(input_extent_is_larger){
    tmp2 <- tempfile(pattern = "crop", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::crop(r, pg, filename = tmp2, gdal=c("COMPRESS=LZW"))
  }

  higher_resolution <- terra::res(r) < terra::res(pg)
  if(any(higher_resolution)){
    tmp3 <- tempfile(pattern = "aggregate", fileext = ".tif", tmpdir = temporary_directory)
    r <- terra::aggregate(r,
                  fact = terra::res(pg)/terra::res(r),
                  fun = agg_fun,
                  filename = tmp3,
                  gdal=c("COMPRESS=LZW"),
                  cores = cores, ...)
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


#' Unzip a File to a Structured Directory (with Safety Check)
#'
#' Extracts the contents of a `.zip` file into a uniquely named subdirectory
#' in the same location as the original file. The output folder is named after
#' the zip file with a `_unzipped` suffix.
#'
#' If the target directory already exists and contains files or subdirectories,
#' the function skips unzipping and returns the existing directory path.
#'
#' @param f Character. Full path to the `.zip` file to be extracted.
#'
#' @return A character string indicating the full path to the directory
#' where the contents were (or would have been) unzipped.
#'
#' @examples
#' \dontrun{
#' zip_path <- "/path/to/hildap_vGLOB-1.0_geotiff.zip"
#' unzipped_dir <- zip_file(zip_path)
#' list.files(unzipped_dir)
#' }
#'
#' @importFrom tools file_path_sans_ext
#' @export
zip_file <- function(f) {
  library(tools)

  if (!file.exists(f)) {
    stop("File not found: ", f)
  }

  zip_name <- basename(f)
  base_name <- file_path_sans_ext(zip_name)
  target_dir <- file.path(dirname(f), paste0(base_name, "_unzipped"))

  # Check if target_dir exists and is non-empty
  if (dir.exists(target_dir)) {
    contents <- list.files(target_dir, all.files = TRUE, no.. = TRUE)
    if (length(contents) > 0) {
      message("Target directory already exists and is not empty: ", target_dir)
      return(target_dir)
    } else {
      message("Target directory exists but is empty, proceeding to unzip...")
    }
  } else {
    dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
    message("Created target directory: ", target_dir)
  }

  # Unzip contents
  message("Unzipping to: ", target_dir)
  unzip(f, exdir = target_dir)

  return(target_dir)
}


