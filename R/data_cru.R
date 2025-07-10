#' Read and Stack CRU Temperature Data from Gzipped NetCDF Files
#'
#' This function locates, decompresses, and reads monthly temperature data from a set of
#' gzipped CRU NetCDF (`.dat.nc.gz`) files. It extracts only the `tmp` (mean temperature) layers,
#' assigns each layer a date based on the filename, renames them using the format `"tmp_YYYY_MM"`,
#' and stacks all rasters into a single `SpatRaster` time-series object.
#'
#' @return A `SpatRaster` object containing all extracted and time-stamped temperature layers.
#'
#' @details
#' The function uses `get_pgfile()` to locate all `.gz` files for CRU TS v4.09 climate data.
#' Each file is decompressed (if not already), and the temperature layers (prefix `"tmp"`)
#' are read using the `terra` package. Timestamps are assigned assuming monthly data
#' starting from the year encoded in the filename. All monthly rasters are merged into a
#' single stack, which is returned.
#'
#' @note
#' This function is tailored to the PRIO-Grid directory structure and assumes that the CRU
#' `.dat.nc.gz` files follow standard CRU TS naming conventions, including the year.
#'
#' @import terra
#' @importFrom R.utils gunzip
#'
#' @examples
#' \dontrun{
#' temp_stack <- read_cru()
#' plot(temp_stack[[1]])
#' }
#'
#' @export


read_cru <- function() {

  # Step 1: Get file paths to all CRU gzipped NetCDFs
  cru_files <- get_pgfile(
    source_name = "CRU Climate",
    source_version = "v4.09",
    id = "9bba83b0-eca9-4f05-b1df-6aeaed55a9fa"
  )

  # Step 2: Process each file: unzip, load, extract 'tmp' layer, name it
  all_rasters <- list()

  for (i in seq_along(cru_files)) {
    gz_file <- cru_files[i]

    filename <- basename(gz_file)
    base_name <- sub("\\.dat\\.nc\\.gz$", "", filename)
    parent_dir <- dirname(gz_file)
    target_dir <- file.path(parent_dir, sub("\\.tmp$", "", base_name))
    dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

    nc_file <- file.path(target_dir, sub("\\.gz$", "", filename))

    # Unzip if needed
    if (!file.exists(nc_file)) {
      message("Decompressing to: ", nc_file)
      R.utils::gunzip(gz_file, destname = nc_file, remove = FALSE, overwrite = TRUE)
    }

    # Load raster and extract only 'tmp' variable
    temp <- rast(nc_file)
    tmp_idx <- grep("^tmp", names(temp))
    temp_only <- temp[[tmp_idx]]

    # Assign time values
    start_year <- as.numeric(sub(".*(\\d{4})\\..*", "\\1", filename))
    dates <- seq(as.Date(sprintf("%d-01-16", start_year)), by = "month", length.out = nlyr(temp_only))
    time(temp_only) <- dates
    names(temp_only) <- paste0("tmp_", format(dates, "%Y_%m"))

    all_rasters[[i]] <- temp_only
  }

  # Step 3: Merge all individual rasters into one SpatRaster
  raster_stack <- do.call(c, all_rasters)
  return(raster_stack)
}

# checked

#' Filter a raster stack by PRIO-GRID date intervals
#'
#' @param raster_stack A RasterStack or SpatRaster with layers named like "tmp_YYYY_MM"
#' @return A list with the filtered raster stack and the filtered layer names
#' @export
filter_raster_by_pg_intervals <- function(raster_stack) {
  # Extract layer names
  layer_names <- names(raster_stack)

  pg_intervals <- pg_date_intervals()

  # Convert to Date objects assuming format "tmp_YYYY_MM"
  layer_dates <- lubridate::ymd(paste0(sub("tmp_", "", layer_names), "-01"))

  # Identify which layers fall within any of the intervals
  keep_layers <- sapply(layer_dates, function(d) any(d %within% pg_intervals))

  # Filter the raster stack
  raster_filtered <- raster_stack[[which(keep_layers)]]

  # Get the filtered layer names
  filtered_names <- names(raster_filtered)

  # Print the first and last layer names
  if (length(filtered_names) > 0) {
    cat("First layer:", filtered_names[1], "\n")
    cat("Last layer:", filtered_names[length(filtered_names)], "\n")
  } else {
    cat("No layers matched the date intervals.\n")
  }

  # Return result
  return(raster_filtered)
}

# checked

# -----OPTIONAL START-----

#' Build Year-Month Table
#'
#' @param start_year Starting year
#' @param start_month Starting month
#' @param end_year Ending year
#' @param end_month Ending month
#' @return Data frame with year, month, and layer_name
#' @export
build_ym_table <- function(start_year, start_month, end_year, end_month) {
  years <- rep(start_year:end_year, each = 12)
  months <- rep(1:12, times = length(start_year:end_year))
  ym_df <- data.frame(year = years, month = months)
  ym_df <- ym_df[with(ym_df, year > start_year | (year == start_year & month >= start_month)), ]
  ym_df <- ym_df[with(ym_df, year < end_year | (year == end_year & month <= end_month)), ]
  ym_df$layer_name <- paste0("tmp_", sprintf("%04d_%02d", ym_df$year, ym_df$month))
  return(ym_df)
}

#' Validate Date Range Against Raster Stack
#'
#' @param start_year Start year
#' @param start_month Start month
#' @param end_year End year
#' @param end_month End month
#' @param available_dates A vector of Dates from the raster stack
#' @return List of start_date and end_date
#' @export
validate_date_range <- function(start_year, start_month, end_year, end_month, available_dates) {
  available_ym <- as.Date(format(available_dates, "%Y-%m-01"))
  first_date <- available_ym[1]
  last_date  <- available_ym[length(available_ym)]

  start_date <- as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  end_date   <- as.Date(sprintf("%04d-%02d-01", end_year, end_month))

  if (start_date < first_date) {
    stop(sprintf("\u274C Start date %s is before first raster date %s", format(start_date, "%Y-%m"), format(first_date, "%Y-%m")))
  }

  if (end_date > last_date) {
    stop(sprintf("\u274C End date %s is after last raster date %s", format(end_date, "%Y-%m"), format(last_date, "%Y-%m")))
  }

  return(list(start_date = start_date, end_date = end_date))
}

# -----OPTIONAL END-----


#' Mask CRU Temperature Raster Stack to PRIO-GRID Extent
#'
#' This function masks a CRU temperature raster stack (`SpatRaster`) to match the valid area
#' defined by a PRIO-GRID raster. Optionally, it can subset specific year-month combinations
#' before masking, based on a user-supplied data frame.
#'
#' @param cru_stack A `SpatRaster` object containing CRU temperature layers with names like "tmp_YYYY_MM".
#' @param pg_grid A `SpatRaster` representing the PRIO-GRID extent and shape. Must have identical resolution,
#' extent, and CRS as `cru_stack`.
#' @param ym_df Optional. A data frame with `year` and `month` columns specifying which layers to subset
#' and mask. If NULL (default), all layers are included.
#'
#' @return A `SpatRaster` object of the same structure as `cru_stack`, but masked to the PRIO-GRID footprint.
#' The resulting raster retains the same layer names and time stamps (e.g., "tmp_2015_06").
#'
#' @details
#' The function ensures geometric alignment between `cru_stack` and `pg_grid`, and applies the PRIO-GRID mask
#' using `terra::mask()`. Layers outside the PRIO-GRID valid area (i.e., cells with NA in `pg_grid`)
#' are set to NA. If `ym_df` is provided, only those year-month combinations are processed.
#'
#' Time metadata is preserved based on the layer names, assigning mid-month dates (e.g., "YYYY-MM-16").
#'
#' @seealso \code{\link[terra]{mask}}, \code{\link[terra]{rast}}, \code{\link[terra]{time}}, \code{\link{pgoptions}}
#' @export
mask_cru_to_pg_stack <- function(cru_stack, pg_grid, ym_df = NULL) {
  library(terra)

  # Ensure alignment
  if (!compareGeom(cru_stack, pg_grid, stopOnError = FALSE)) {
    stop("Geometry mismatch: 'cru_stack' and 'pg_grid' must have identical extent, resolution, and CRS.")
  }

  # Get full layer info
  all_names <- names(cru_stack)
  all_dates <- time(cru_stack)

  full_ym_df <- data.frame(
    layer_name = all_names,
    year = as.integer(format(all_dates, "%Y")),
    month = as.integer(format(all_dates, "%m")),
    stringsAsFactors = FALSE
  )

  # Subset if ym_df is provided
  if (!is.null(ym_df)) {
    ym_df$layer_name <- paste0("tmp_", sprintf("%04d", ym_df$year), "_", sprintf("%02d", ym_df$month))
    full_ym_df <- merge(full_ym_df, ym_df, by = "layer_name")

    # Drop duplicate .y columns and keep only merged year/month
    full_ym_df$year <- full_ym_df$year.x
    full_ym_df$month <- full_ym_df$month.x
    full_ym_df <- full_ym_df[, c("layer_name", "year", "month")]
  }

  # Prepare output list
  masked_layers <- list()

  # Create mask once
  pg_mask <- !is.na(values(pg_grid))

  # Loop through selected layers
  for (i in seq_len(nrow(full_ym_df))) {
    lyr_name <- full_ym_df$layer_name[i]
    r <- cru_stack[[lyr_name]]
    r_masked <- mask(r, pg_grid)

    # Explicitly drop values outside PRIO-GRID
    vals <- values(r_masked)
    vals[!pg_mask] <- NA
    values(r_masked) <- vals

    names(r_masked) <- lyr_name
    masked_layers[[i]] <- r_masked
  }

  # Combine into stack
  masked_stack <- rast(masked_layers)

  # Preserve time if available
  if (!is.null(all_dates)) {
    matched_dates <- full_ym_df[match(names(masked_stack), full_ym_df$layer_name), ]
    time(masked_stack) <- as.Date(sprintf("%04d-%02d-16", matched_dates$year, matched_dates$month))
  }

  return(masked_stack)
}

# checked

#' Aggregate a raster stack to pgoptions$get_temporal_resolution(), if needed
#'
#' @param raster_stack A SpatRaster with layers named like "tmp_YYYY_MM"
#' @param fun Aggregation function, default is mean
#' @return A SpatRaster aggregated over pg_date_intervals(), or original raster if no change needed
#' @export
aggregate_if_needed <- function(raster_stack, fun = mean) {
  # Extract dates from layer names
  layer_names <- names(raster_stack)
  layer_dates <- lubridate::ymd(paste0(sub("tmp_", "", layer_names), "-01"))

  # Check native resolution: most common difference between consecutive dates
  delta_days <- diff(layer_dates)
  native_resolution <- as.numeric(median(delta_days))  # In days

  # Get desired resolution in days
  temporal_setting <- pgoptions$get_temporal_resolution()
  pg_intervals <- pg_date_intervals()
  desired_resolution_days <- as.numeric(median(int_length(pg_intervals)) / (60 * 60 * 24))  # seconds to days

  # If aggregation is needed
  if (native_resolution < desired_resolution_days) {
    message("Aggregating from ~", round(native_resolution), " day resolution to ", temporal_setting)

    # Assign each date to a pg_interval index
    interval_index <- sapply(layer_dates, function(d) {
      match(TRUE, d %within% pg_intervals, nomatch = NA_integer_)
    })

    valid_idxs <- which(!is.na(interval_index))
    raster_stack <- raster_stack[[valid_idxs]]
    interval_index <- interval_index[valid_idxs]

    grouped <- split(seq_along(interval_index), interval_index)

    # Aggregate layers per interval
    aggregated <- lapply(seq_along(grouped), function(i) {
      lyr_idxs <- grouped[[i]]
      agg_layer <- terra::app(raster_stack[[lyr_idxs]], fun = fun, na.rm = TRUE)

      # Format the interval string (drop UTC)
      interval <- pg_intervals[i]
      interval_label <- paste0(
        format(int_start(interval), "%Y-%m-%d"), "--",
        format(int_end(interval), "%Y-%m-%d")
      )

      names(agg_layer) <- interval_label
      agg_layer
    })

    result_stack <- do.call(c, aggregated)
    return(result_stack)
  } else {
    message("No temporal aggregation needed.")
    return(raster_stack)
  }
}

# checked

#' Plot PRIO-GRID Raster Stack to Multi-Page PDF
#'
#' This function creates a multi-page PDF containing tiled plots of raster layers from a PRIO-GRID-aligned
#' CRU temperature raster stack. It saves the PDF to a `/reports` folder located in the same directory as the source files.
#'
#' @param cru_stack_check_for_temp_aggregation A `SpatRaster` object (e.g., masked and aggregated CRU temperature data).
#' @param f A character vector of file paths (e.g., the original `.gz` or `.nc` file list).
#' Only the first element is used to determine the output location.
#' @param nrow Number of rows per page of plots. Default is 3.
#' @param ncol Number of columns per page of plots. Default is 4.
#'
#' @return A side effect: A PDF file saved to the `/reports` directory with one or more pages of raster plots.
#' No object is returned.
#'
#' @details
#' The function automatically creates a `reports/` folder in the same parent directory as the first file
#' in `f`. It arranges plots using `par(mfrow = ...)`, and splits the raster stack across multiple pages if needed.
#'
#' This is useful for visually inspecting large raster stacks (e.g., for checking alignment, data quality, or naming).
#'
#' @seealso \code{\link[terra]{plot}}, \code{\link[grDevices]{pdf}}, \code{\link[terra]{nlyr}}
#' @export
plot_pg_stack_to_pdf <- function(cru_stack_check_for_temp_aggregation, f, nrow = 3, ncol = 4) {
  library(terra)

  # Derive base output directory from f[1]
  base_dir <- dirname(f[1])
  report_dir <- file.path(base_dir, "reports")
  print(report_dir)

  # Create 'reports' directory if it doesn't exist
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE)
  }

  # Define final PDF path
  output_pdf <- file.path(report_dir, "CRU_pg_review.pdf")

  nlayers_total <- nlyr(cru_stack_check_for_temp_aggregation)
  layer_names <- names(cru_stack_check_for_temp_aggregation)

  # Calculate number of pages needed
  layers_per_page <- nrow * ncol
  n_pages <- ceiling(nlayers_total / layers_per_page)

  # Start PDF
  pdf(output_pdf, width = 11, height = 8.5)

  for (i in seq_len(n_pages)) {
    start_idx <- (i - 1) * layers_per_page + 1
    end_idx <- min(i * layers_per_page, nlayers_total)
    layer_subset <- cru_stack_check_for_temp_aggregation[[start_idx:end_idx]]

    par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 2))

    for (j in seq_len(nlyr(layer_subset))) {
      lyr <- layer_subset[[j]]
      plot(lyr, main = names(layer_subset)[j])
    }
  }

  dev.off()
  message("PDF saved to: ", output_pdf)
}

# checked

#' Wrapper: End-to-End CRU Loading, Masking, Filtering, Aggregation, and Plotting
#'
#' This wrapper function handles the full workflow of processing CRU climate data:
#' loading source files, stacking rasters, filtering by PRIO-GRID intervals,
#' masking to the PRIO-GRID extent, optionally aggregating temporally, and generating a PDF of plots.
#'
#' @param start_year Optional. Start year for subsetting the output stack.
#' @param start_month Optional. Start month for subsetting the output stack.
#' @param end_year Optional. End year for subsetting the output stack.
#' @param end_month Optional. End month for subsetting the output stack.
#'
#' @return A `SpatRaster` object containing the fully processed, PRIO-aligned CRU temperature stack.
#' The result may be filtered, masked, and temporally aggregated.
#'
#' @export
process_cru_to_pg_stack <- function(start_year = NULL, start_month = NULL, end_year = NULL, end_month = NULL) {
  # 1. Read file paths to CRU data
  cru_stack <- read_cru()

  #----REVISED SO read_cru() PROVIDES A RASTER STACK----
  # 2. Load individual CRU files (as SpatRaster)
  #cru_temp <- load_all_cru_temperature(f)

  # 3. Stack the temperature data from the extracted NetCDFs
  #cru_stack <- stack_cru_temperature_from_sources(f)
  #-----------------------------------------------------

  # 4. Filter the raster stack by PRIO-GRID date intervals
  cru_stack_filtered <- filter_raster_by_pg_intervals(cru_stack)

  # 5. Load the PRIO-GRID spatial mask
  pg_grid <- prio_blank_grid()

  # 6. Apply the spatial mask (with optional date subsetting)
  if (!is.null(start_year) && !is.null(start_month) && !is.null(end_year) && !is.null(end_month)) {
    validate_date_range(start_year, start_month, end_year, end_month, time(cru_stack_filtered))
    ym_df <- build_ym_table(start_year, start_month, end_year, end_month)
    pg_stack <- mask_cru_to_pg_stack(cru_stack_filtered, pg_grid, ym_df)
  } else {
    pg_stack <- mask_cru_to_pg_stack(cru_stack_filtered, pg_grid)
  }

  # 7. Aggregate temporally if needed (e.g., from daily to monthly or annual)
  cru_stack_check_for_temp_aggregation <- aggregate_if_needed(pg_stack)

  # 8. Create a multi-page PDF for visual inspection
  plot_pg_stack_to_pdf(cru_stack_check_for_temp_aggregation, f)

  # 9. Return final processed stack
  return(cru_stack_check_for_temp_aggregation)
}




