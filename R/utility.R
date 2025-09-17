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



  unit <- dplyr::case_when(
    grepl("month", temporal_resolution) ~ "month",
    grepl("week", temporal_resolution) ~ "week",
    grepl("quarter", temporal_resolution) ~ "quarter",
    grepl("year", temporal_resolution) ~ "year"
  )

  is_end_of_month <- start_date == (lubridate::ceiling_date(start_date, "month") - lubridate::days(1))

  base_seq <- pg_dates(start_date, end_date, temporal_resolution)
  floor <- lubridate::floor_date(base_seq, unit)
  #ceil <- lubridate::ceiling_date(base_seq, unit) - lubridate::days(1)

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
  df <- df[rowSums(!is.na(df)) > 1,] # remove rows with only missing elements.
  names(df)

  if(static){
    return(df)
  } else{
    # Assumes variable names in raster are dates.
    df <- tidyr::pivot_longer(df,
                              cols = -dplyr::all_of(c("pgid")),
                              names_to = "measurement_date",
                              values_to = varname,
                              names_transform = as.Date)
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
#' @param cores Integer specifying number of CPU cores to use for aggregation
#'   operations. Defaults to 1. Higher values can speed up processing of large datasets.
#' @param ... Additional arguments passed to \code{\link[terra]{aggregate}}.
#'   Useful for controlling aggregation behavior (e.g., na.rm = TRUE).
#'
#' @return SpatRaster object conforming to PRIO-GRID specifications:
#' \itemize{
#'   \item CRS: As specified in global options (default: EPSG:4326)
#'   \item Extent: As specified in global options (default: global extent)
#'   \item Resolution: Calculated from nrow/ncol in global options
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
#' @section Global Options:
#' This function uses global PRIO-GRID options set via PGOptionsManager:
#' \itemize{
#'   \item \code{pgoptions$get_rawfolder()}: Location for temporary file storage
#'   \item \code{pgoptions$get_ncol()}, \code{pgoptions$get_nrow()}: Output grid dimensions
#'   \item \code{pgoptions$get_extent()}: Output spatial extent
#'   \item \code{pgoptions$get_crs()}: Output coordinate reference system
#' }
#'
#' @examples
#' \dontrun{
#' # Downloads and transfomrs GHSL to PRIO-GRID resolution
#' download_pg_rawdata(pg_rawfiles() |> dplyr::filter(id == "ae6a7612-4bef-452f-acd6-d2212cf9a7c5"))
#' r <- read_ghsl_population_grid()
#' res <- robust_transformation(r, agg_fun = "sum")
#' }
#'
#' @seealso
#' \code{\link{prio_blank_grid}} for creating empty PRIO-GRID templates,
#' \code{\link{PGOptionsManager}} for setting global options,
#' \code{\link[terra]{aggregate}}, \code{\link[terra]{disagg}}, \code{\link[terra]{resample}}
#'
#' @export
robust_transformation <- function(r, agg_fun, disagg_method = "near", ...){
  pg <- prio_blank_grid()
  temporary_directory <- file.path(pgoptions$get_rawfolder(), "tmp", tempdir() |> basename())
  dir.create(temporary_directory)

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
      #Target directory already exists and is not empty
      return(target_dir)
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


