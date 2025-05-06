#' Load and Prepare CRU Temperature Raster from File List
#'
#' Reads a .txt file containing NetCDF filenames, selects one by index,
#' downloads it if missing, decompresses it if needed, and loads the temperature data.
#'
#' @param txt_path Path to .txt file listing NetCDF filenames (one per line).
#' @param index Index of the file to process (1-based).
#' @param nc_dir Local directory where files should be saved/loaded (e.g., "CRU_Processed").
#'
#' @return A SpatRaster object with temperature layers and date-aware layer names.
load_cru_temperature <- function(txt_path, index, nc_dir = "../ClimateData_Notebooks/CRU_Processed") {
  library(terra)
  library(R.utils)

  # Read file list
  file_list <- readLines(txt_path)

  # Validate index
  if (index < 1 || index > length(file_list)) {
    stop(sprintf("Index %d is out of bounds. Found %d files.", index, length(file_list)))
  }

  # Ensure output directory exists
  if (!dir.exists(nc_dir)) dir.create(nc_dir, recursive = TRUE)

  filename <- file_list[index]
  base_url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/cruts.2503051245.v4.09/tmp/"
  url <- paste0(base_url, filename)
  local_gz <- file.path(nc_dir, filename)
  local_nc <- sub("\\.gz$", "", local_gz)

  # Download if needed
  if (!file.exists(local_gz) && !file.exists(local_nc)) {
    message("🔽 Downloading: ", filename)
    download.file(url, destfile = local_gz, mode = "wb")
  }

  # Decompress if necessary
  if (!file.exists(local_nc)) {
    message("📦 Decompressing: ", filename)
    gunzip(local_gz, destname = local_nc, remove = FALSE, overwrite = TRUE)
  }

  # Load NetCDF file
  temp <- rast(local_nc)
  tmp_idx <- grep("^tmp", names(temp))
  temp_only <- temp[[tmp_idx]]

  # Parse start year from filename
  start_year <- as.numeric(sub(".*(\\d{4})\\..*", "\\1", filename))
  dates <- seq(as.Date(sprintf("%d-01-16", start_year)), by = "month", length.out = nlyr(temp_only))

  # Annotate raster with time and names
  time(temp_only) <- dates
  names(temp_only) <- paste0("tmp_", format(dates, "%Y_%m"))

  return(temp_only)
}



#' Validate User-Defined Date Range Against Available CRU Dates
#'
#' Ensures the specified year/month window is within the range
#' of available raster time layers, ignoring day component.
#'
#' @param start_year Start year as numeric.
#' @param start_month Start month (1–12).
#' @param end_year End year as numeric.
#' @param end_month End month (1–12).
#' @param available_dates Vector of `Date` objects from the raster.
#'
#' @return A list with `start_date` and `end_date` as year-month `Date` objects.
validate_date_range <- function(start_year, start_month, end_year, end_month, available_dates) {
  # Convert all dates to year-month format (first day of month)
  available_ym <- as.Date(format(available_dates, "%Y-%m-01"))
  first_date <- available_ym[1]
  last_date  <- available_ym[length(available_ym)]

  start_date <- as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  end_date   <- as.Date(sprintf("%04d-%02d-01", end_year, end_month))

  if (start_date < first_date) {
    stop(sprintf("❌ Start date %s is before first raster date %s", format(start_date, "%Y-%m"), format(first_date, "%Y-%m")))
  }

  if (end_date > last_date) {
    stop(sprintf("❌ End date %s is after last raster date %s", format(end_date, "%Y-%m"), format(last_date, "%Y-%m")))
  }

  return(list(start_date = start_date, end_date = end_date))
}



#' Build a Year-Month Table for Extraction Loop
#'
#' Creates a table of all month/year combinations between two endpoints.
#'
#' @param start_year Start year.
#' @param start_month Start month.
#' @param end_year End year.
#' @param end_month End month.
#'
#' @return A `data.frame` with columns `year`, `month`, and `layer_name`.
build_ym_table <- function(start_year, start_month, end_year, end_month) {
  years <- rep(start_year:end_year, each = 12)
  months <- rep(1:12, times = length(start_year:end_year))
  ym_df <- data.frame(year = years, month = months)
  ym_df <- ym_df[with(ym_df, year > start_year | (year == start_year & month >= start_month)), ]
  ym_df <- ym_df[with(ym_df, year < end_year | (year == end_year & month <= end_month)), ]
  ym_df$layer_name <- paste0("tmp_", sprintf("%04d_%02d", ym_df$year, ym_df$month))
  return(ym_df)
}


#' Extract Zonal Mean Temperature for PRIO-GRID Cells
#'
#' Loops over monthly layers and extracts mean raster values per PG cell.
#'
#' @param temp_stack A `SpatRaster` with renamed and timestamped layers.
#' @param pg_sf An `sf` object representing PRIO-GRID polygons.
#' @param ym_df A `data.frame` with `year`, `month`, and `layer_name`.
#'
#' @return A tidy `data.frame` with `pg_id`, `year`, `month`, and `tmp_value`.
extract_cru_to_pg <- function(temp_stack, pg_sf, ym_df) {
  pg_terra <- vect(pg_sf)
  out_df <- data.frame()

  for (i in seq_len(nrow(ym_df))) {
    lyr_name <- ym_df$layer_name[i]
    if (!(lyr_name %in% names(temp_stack))) next
    r <- temp_stack[[lyr_name]]
    extracted <- terra::extract(r, pg_terra, fun = mean, na.rm = TRUE)
    df <- data.frame(
      pg_id = pg_sf$pg_id,
      year = ym_df$year[i],
      month = ym_df$month[i],
      tmp_value = extracted[[2]]
    )
    out_df <- bind_rows(out_df, df)
  }

  return(out_df)
}


#' End-to-End CRU Temperature Extraction Pipeline
#'
#' Loads, validates, and extracts CRU climate data for a specified date range
#' using PRIO-GRID polygons. Selects a NetCDF file based on index in a .txt list.
#'
#' @param file_list_path Path to a .txt file containing a list of NetCDF filenames (one per line).
#' @param file_index Index of the file in the list to load.
#' @param nc_dir Directory where the NetCDF files are stored (default is ".").
#' @param pg_sf An `sf` object containing PRIO-GRID geometries.
#' @param start_year Start year (numeric).
#' @param start_month Start month (numeric 1–12).
#' @param end_year End year (numeric).
#' @param end_month End month (numeric 1–12).
#'
#' @return A tidy `data.frame` with columns: `pg_id`, `year`, `month`, and `tmp_value`.
process_cru_to_pg <- function(file_list_path, file_index, nc_dir = "../ClimateData_Notebooks/CRU_Processed/", pg_sf,
                              start_year, start_month, end_year, end_month) {
  library(terra)
  library(sf)
  library(dplyr)
  library(lubridate)
  library(R.utils)

  # Step 1: Load CRU raster from selected file
  temp_only <- load_cru_temperature(txt_path = file_list_path, index = file_index, nc_dir = nc_dir)

  # Step 2: Validate date range is compatible with the raster
  validate_date_range(start_year, start_month, end_year, end_month, time(temp_only))

  # Step 3: Build year-month table
  ym_df <- build_ym_table(start_year, start_month, end_year, end_month)

  # Step 4: Extract climate values into tidy format
  climate_df <- extract_cru_to_pg(temp_only, pg_sf, ym_df)

  return(climate_df)
}
