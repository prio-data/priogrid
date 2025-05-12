#' Read CRU Data File Paths
#'
#' Retrieves the paths to gzipped CRU temperature files.
#'
#' @return A character vector of file paths
#' @export
#' @references \insertRef{harrisVersion4CRU2020}{priogrid}
read_cru <- function() {
  f <- get_pgfile(source_name = "CRU Climate",
                  source_version = "v4.09",
                  id = "d9ffee47-4e83-4805-9b3f-5e0e889fd2db")
  return(f)
}

#' Load a Single CRU Temperature File
#'
#' Decompresses and loads a gzipped NetCDF file containing temperature data.
#'
#' @param index The index of the file to load from the 'sources' vector
#' @param sources A character vector of gzipped CRU file paths
#' @return A SpatRaster object containing temperature layers
#' @export
load_cru_temperature <- function(index, sources) {
  library(terra)
  library(R.utils)
  library(tools)

  if (index < 1 || index > length(sources)) {
    stop(sprintf("Index %d is out of bounds. Found %d sources.", index, length(sources)))
  }

  gz_file <- sources[index]
  filename <- basename(gz_file)
  base_name <- sub("\\.dat\\.nc\\.gz$", "", filename)
  parent_dir <- dirname(gz_file)
  target_dir <- file.path(parent_dir, sub("\\.tmp$", "", base_name))
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

  nc_file <- file.path(target_dir, sub("\\.gz$", "", filename))
  if (!file.exists(nc_file)) {
    message("\U0001F4E6 Decompressing to: ", nc_file)
    gunzip(gz_file, destname = nc_file, remove = FALSE, overwrite = TRUE)
  }

  temp <- rast(nc_file)
  tmp_idx <- grep("^tmp", names(temp))
  temp_only <- temp[[tmp_idx]]

  start_year <- as.numeric(sub(".*(\\d{4})\\..*", "\\1", filename))
  dates <- seq(as.Date(sprintf("%d-01-16", start_year)), by = "month", length.out = nlyr(temp_only))
  time(temp_only) <- dates
  names(temp_only) <- paste0("tmp_", format(dates, "%Y_%m"))

  return(temp_only)
}

#' Load All CRU Temperature Files
#'
#' Loads and combines all CRU temperature raster files.
#'
#' @param sources A character vector of gzipped CRU file paths
#' @return A combined SpatRaster object with all time layers
#' @export
load_all_cru_temperature <- function(sources) {
  message("Processing ", length(sources), " CRU files...")

  temp_list <- lapply(seq_along(sources), function(i) {
    message("Loading file ", i, " of ", length(sources))
    load_cru_temperature(i, sources)
  })

  combined <- do.call(c, temp_list)
  return(combined)
}

#' Stack All Unzipped CRU Temperature Files
#'
#' Reads and concatenates all .nc files from a parent directory.
#'
#' @param sources A vector of CRU file paths
#' @param save_name Filename to save the stack (default: 'cru_stack')
#' @param plot_layer Optional name of a layer to plot
#' @return A SpatRaster stack
#' @export
stack_cru_temperature_from_sources <- function(sources, save_name = 'cru_stack', plot_layer = "tmp_2020_01") {
  library(terra)

  if (length(sources) == 0) stop("\u274C 'sources' is empty.")
  parent_dir <- dirname(sources[1])
  message("\U0001F4C1 Searching unzipped .nc files in: ", parent_dir)

  nc_files <- list.files(parent_dir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  if (length(nc_files) == 0) stop("\u274C No .nc files found in: ", parent_dir)

  load_nc_file <- function(nc_path) {
    temp <- rast(nc_path)
    tmp_idx <- grep("^tmp", names(temp))
    temp_only <- temp[[tmp_idx]]
    start_year <- as.numeric(sub(".*(\\d{4})\\..*", "\\1", basename(nc_path)))
    dates <- seq(as.Date(sprintf("%d-01-16", start_year)), by = "month", length.out = nlyr(temp_only))
    time(temp_only) <- dates
    names(temp_only) <- paste0("tmp_", format(dates, "%Y_%m"))
    return(temp_only)
  }

  temp_stack_list <- lapply(nc_files, load_nc_file)
  cru_temp_stack <- do.call(c, temp_stack_list)
  message("\u2705 Raster stack created with ", nlyr(cru_temp_stack), " monthly layers.")

  save_path <- file.path(parent_dir, paste0(save_name, ".tif"))
  writeRaster(cru_temp_stack, save_path, overwrite = TRUE)
  message("\U0001F4BE Saved raster to: ", save_path)

  if (plot_layer %in% names(cru_temp_stack)) {
    plot(cru_temp_stack[[plot_layer]], main = paste("CRU Monthly Temperature -", plot_layer))
  } else {
    warning("\u26A0\uFE0F Layer '", plot_layer, "' not found in stack.")
  }

  return(cru_temp_stack)
}

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

#' Mask CRU Stack to PRIO-GRID Raster
#'
#' @param cru_stack A SpatRaster of CRU monthly temperature layers
#' @param pg_grid A PRIO-GRID raster with 1:1 unique values per cell
#' @param ym_df Optional year-month filter (data.frame with year/month)
#' @return Masked raster stack
#' @export
mask_cru_to_pg_stack <- function(cru_stack, pg_grid, ym_df = NULL) {
  library(terra)
  if (!compareGeom(cru_stack, pg_grid, stopOnError = FALSE)) {
    stop("Geometry mismatch: 'cru_stack' and 'pg_grid' must have identical extent, resolution, and CRS.")
  }

  all_names <- names(cru_stack)
  all_dates <- time(cru_stack)
  full_ym_df <- data.frame(
    layer_name = all_names,
    year = as.integer(format(all_dates, "%Y")),
    month = as.integer(format(all_dates, "%m")),
    stringsAsFactors = FALSE
  )

  if (!is.null(ym_df)) {
    ym_df$layer_name <- paste0("tmp_", sprintf("%04d", ym_df$year), "_", sprintf("%02d", ym_df$month))
    full_ym_df <- merge(full_ym_df, ym_df, by = "layer_name")
    full_ym_df$year <- full_ym_df$year.x
    full_ym_df$month <- full_ym_df$month.x
    full_ym_df <- full_ym_df[, c("layer_name", "year", "month")]
  }

  masked_layers <- list()
  pg_mask <- !is.na(values(pg_grid))

  for (i in seq_len(nrow(full_ym_df))) {
    lyr_name <- full_ym_df$layer_name[i]
    r <- cru_stack[[lyr_name]]
    r_masked <- mask(r, pg_grid)
    vals <- values(r_masked)
    vals[!pg_mask] <- NA
    values(r_masked) <- vals
    names(r_masked) <- lyr_name
    masked_layers[[i]] <- r_masked
  }

  masked_stack <- rast(masked_layers)
  matched_dates <- full_ym_df[match(names(masked_stack), full_ym_df$layer_name), ]
  time(masked_stack) <- as.Date(sprintf("%04d-%02d-16", matched_dates$year, matched_dates$month))

  return(masked_stack)
}

#' Plot Raster Stack to PDF Grid
#'
#' @param pg_stack A SpatRaster stack (e.g., masked CRU temperature)
#' @param f A character vector of original .gz source paths
#' @param nrow Number of rows per PDF page
#' @param ncol Number of columns per PDF page
#' @export
plot_pg_stack_to_pdf <- function(pg_stack, f, nrow = 3, ncol = 4) {
  library(terra)
  base_dir <- dirname(f[1])
  report_dir <- file.path(base_dir, "reports")
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE)
  }
  output_pdf <- file.path(report_dir, "CRU_pg_review.pdf")

  nlayers_total <- nlyr(pg_stack)
  layers_per_page <- nrow * ncol
  n_pages <- ceiling(nlayers_total / layers_per_page)
  pdf(output_pdf, width = 11, height = 8.5)

  for (i in seq_len(n_pages)) {
    start_idx <- (i - 1) * layers_per_page + 1
    end_idx <- min(i * layers_per_page, nlayers_total)
    layer_subset <- pg_stack[[start_idx:end_idx]]
    par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 2))
    for (j in seq_len(nlyr(layer_subset))) {
      lyr <- layer_subset[[j]]
      plot(lyr, main = names(layer_subset)[j])
    }
  }
  dev.off()
  message("\u2705 PDF saved to: ", output_pdf)
}

#' Wrapper: End-to-End CRU Loading, Masking, and Plotting
#'
#' @param start_year Start year (optional)
#' @param start_month Start month (optional)
#' @param end_year End year (optional)
#' @param end_month End month (optional)
#' @export
process_cru_to_pg_stack <- function(start_year = NULL, start_month = NULL, end_year = NULL, end_month = NULL) {
  f <- read_cru()
  cru_temp <- load_all_cru_temperature(f)
  cru_stack <- stack_cru_temperature_from_sources(f)
  pg_grid <- prio_blank_grid()

  if (!is.null(start_year) && !is.null(start_month) && !is.null(end_year) && !is.null(end_month)) {
    validate_date_range(start_year, start_month, end_year, end_month, time(cru_stack))
    ym_df <- build_ym_table(start_year, start_month, end_year, end_month)
    pg_stack <- mask_cru_to_pg_stack(cru_temp, pg_grid, ym_df)
  } else {
    pg_stack <- mask_cru_to_pg_stack(cru_temp, pg_grid)
  }

  plot_pg_stack_to_pdf(pg_stack, f)

  return(pg_stack)
}
