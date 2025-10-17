# Helper operator for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Get spatial configuration hash
#'
#' Creates a 6-character MD5 hash of the current spatial options from pgoptions.
#'
#' @return Character string with 6-character hash
#' @keywords internal
get_spatial_hash <- function() {
  spatial_options <- list(
    nrow = pgoptions$get_nrow(),
    ncol = pgoptions$get_ncol(),
    crs = pgoptions$get_crs(unparsed = TRUE),
    extent = as.vector(pgoptions$get_extent())
  )

  hash_input <- paste(spatial_options, collapse = "_")
  digest::digest(hash_input, algo = "md5") |> substr(1, 6)
}

#' Get temporal configuration hash
#'
#' Creates a 6-character MD5 hash of the current temporal options from pgoptions.
#' Rounds end_date to avoid creating new hashes for small date changes.
#'
#' @return Character string with 6-character hash
#' @keywords internal
get_temporal_hash <- function() {
  # Avoid building new hash for small changes in end date.
  # Round to first date given temporal resolution.
  rounded_end_date <- lubridate::floor_date(
    pgoptions$get_end_date(),
    pgoptions$get_temporal_resolution()
  )

  temporal_options <- list(
    temporal_resolution = pgoptions$get_temporal_resolution(),
    start_date = pgoptions$get_start_date(),
    end_date = rounded_end_date
  )

  hash_input <- paste(temporal_options, collapse = "_")
  digest::digest(hash_input, algo = "md5") |> substr(1, 6)
}


#' Path to store PRIOGRID results
#'
#' Returns the file path where PRIOGRID data is stored. The path structure depends
#' on whether you're working with custom data or official releases.
#'
#' @param version Character string specifying PRIOGRID version. Use for official releases.
#' @param type Character string specifying release type (e.g., "05deg_yearly"). Use for official releases.
#' @param spatial_hash Character string with 6-character spatial hash. If NULL, computed from current pgoptions.
#' @param temporal_hash Character string with 6-character temporal hash. If NULL, computed from current pgoptions.
#'
#' @return Character string with file path
#' @export
#'
#' @examples
#' \dontrun{
#'   # Custom data with current pgoptions
#'   pgout_path()
#'
#'   # Custom data with specific hashes
#'   pgout_path(spatial_hash = "a3f2e1", temporal_hash = "9c8b7a")
#'
#'   # Official release
#'   pgout_path(version = "3.0.0", type = "05deg_yearly")
#' }
pgout_path <- function(version = NULL,
                       type = NULL,
                       spatial_hash = NULL,
                       temporal_hash = NULL) {
  base_path <- file.path(pgoptions$get_rawfolder(), "priogrid")

  if (!is.null(type)) {
    # Release mode
    if (is.null(version)) {
      stop("Release mode requires both 'version' and 'type' parameters")
    }
    return(file.path(base_path, "releases", version, type))
  }

  # Custom mode
  pkg_version <- version %||% packageVersion("priogrid")
  s_hash <- spatial_hash %||% get_spatial_hash()
  t_hash <- temporal_hash %||% get_temporal_hash()

  file.path(base_path, "custom", pkg_version, s_hash, t_hash)
}


#' Calculate PRIO-GRID variables
#'
#' Calculates PRIO-GRID variables based on current pgoptions and saves them
#' to disk. Each variable is computed by calling its corresponding gen_*() function
#' and saved as an .rds file in the custom data folder.
#'
#' @param varnames Character vector with variable names from [pgvariables].
#'   If NULL (default), calculates all available variables.
#' @param overwrite Logical. If FALSE (default), skips variables that already
#'   exist in the output folder. If TRUE, recalculates all specified variables.
#'
#' @return NULL (invisibly). Called for side effects (saving files).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Calculate single variable
#'   calc_pg("ne_disputed_area_share")
#'
#'   # Calculate all variables
#'   calc_pg()
#'
#'   # Recalculate existing variables
#'   calc_pg(overwrite = TRUE)
#' }
calc_pg <- function(varnames = NULL, overwrite = FALSE) {
  if (is.null(varnames)) {
    varnames <- pgvariables$name
  }

  valid_varnames <- varnames[varnames %in% pgvariables$name]
  invalid_varnames <- varnames[!varnames %in% pgvariables$name]
  if (length(invalid_varnames) > 0) {
    message(paste("Ignored varnames:", paste(invalid_varnames, collapse = ", ")))
  }

  if (length(valid_varnames) == 0) {
    stop("No valid varnames supplied. Use names in `pgvariables`")
  }

  save_to <- pgout_path()

  if (!dir.exists(save_to)) {
    dir.create(save_to, recursive = TRUE)
  }

  existing_files <- list.files(save_to) |> tools::file_path_sans_ext()

  if (!overwrite) {
    valid_varnames <- valid_varnames[!valid_varnames %in% existing_files]
  }

  if (length(valid_varnames) == 0) {
    message("All requested variables already exist. Use overwrite=TRUE to recalculate.")
    return(invisible(NULL))
  }

  # Calculate variables
  message(paste("Variables to calculate:", paste(valid_varnames, collapse = ", ")))
  message(paste("Saving to:", save_to))
  for (varname in valid_varnames) {
    message(paste("Calculating", varname))
    r <- get(paste0("gen_", varname))()
    save_pgvariable(r, varname, save_to = save_to)
  }

  invisible(NULL)
}

#' Save a PRIO-GRID variable
#'
#' Saves a terra SpatRaster as a wrapped .rds file. The variable must be
#' listed in [pgvariables].
#'
#' @param rast Terra SpatRaster object from a gen_*() function
#' @param varname Character string with the variable name (must exist in pgvariables)
#' @param save_to Character string with folder path. Defaults to current custom
#'   data location based on pgoptions.
#'
#' @return NULL (invisibly). Called for side effects (saving file).
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- gen_ne_disputed_area_share()
#'   save_pgvariable(r, "ne_disputed_area_share")
#' }
save_pgvariable <- function(rast, varname, save_to = pgout_path()) {
  if (!varname %in% pgvariables$name) {
    stop("varname '", varname, "' not found in pgvariables.")
  }

  if (!dir.exists(save_to)) {
    stop("Output directory does not exist: ", save_to)
  }

  filepath <- file.path(save_to, paste0(varname, ".rds"))

  if (!inherits(rast, "PackedSpatRaster")) {
    rast <- terra::wrap(rast)
  }

  saveRDS(rast, filepath)
  invisible(NULL)
}

#' Load a PRIO-GRID variable
#'
#' Loads a PRIO-GRID variable from disk and returns it as a terra SpatRaster.
#' Can load from custom data (using pgoptions or specific hashes) or from
#' official releases.
#'
#' @param varname Character string with the variable name
#' @param version Character string specifying PRIOGRID version. Relevant for loading official releases.
#' @param type Character string specifying release type (e.g., "05deg_yearly").
#' @param spatial_hash Character string with 6-character spatial hash. If NULL, uses current pgoptions.
#' @param temporal_hash Character string with 6-character temporal hash. If NULL, uses current pgoptions.
#'
#' @return Terra SpatRaster object
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load from current pgoptions
#'   r <- load_pgvariable("ne_disputed_area_share")
#'
#'   # Load from specific custom configuration
#'   r <- load_pgvariable("ne_disputed_area_share",
#'                        spatial_hash = "a3f2e1",
#'                        temporal_hash = "9c8b7a")
#'
#'   # Load from official release
#'   r <- load_pgvariable("ne_disputed_area_share",
#'                        version = "3.0.0",
#'                        type = "05deg_yearly")
#' }
load_pgvariable <- function(varname,
                            version = NULL,
                            type = NULL,
                            spatial_hash = NULL,
                            temporal_hash = NULL) {

  filepath <- file.path(
    pgout_path(version = version,
               type = type,
               spatial_hash = spatial_hash,
               temporal_hash = temporal_hash),
    paste0(varname, ".rds")
  )

  if (!file.exists(filepath)) {
    stop("Variable '", varname, "' not found at: ", filepath)
  }

  terra::unwrap(readRDS(filepath))
}

#' Collect static (non-time-varying) PRIO-GRID data
#'
#' Loads all static variables and returns them either as a data.table or as
#' a list of rasters. See [pgvariables] for available variables.
#'
#' @param version Character string specifying PRIOGRID version.
#' @param type Character string specifying release type (e.g., "05deg_yearly").
#' @param spatial_hash Character string with 6-character spatial hash (custom only).
#' @param temporal_hash Character string with 6-character temporal hash (custom only).
#' @param as_raster Logical. If TRUE, returns list of SpatRasters. If FALSE
#'   (default), returns data.table.
#' @param test Logical. If TRUE, prints coverage summary for each variable.
#' @param overwrite Logical. If FALSE (default) and cached file exists, returns
#'   cached data. If TRUE, rebuilds from individual variables.
#'
#' @return data.table with pgid as rows and variables as columns, or list of
#'   terra SpatRasters if as_raster=TRUE
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load as data.table
#'   pg_dt <- read_pg_static()
#'
#'   # Test coverage
#'   pg_dt <- read_pg_static(test = TRUE)
#'
#'   # Load as rasters
#'   pg_rast <- read_pg_static(as_raster = TRUE)
#'
#'   # Load official release
#'   pg_dt <- read_pg_static(version = "3.0.0",
#'                           type = "05deg_yearly")
#' }
read_pg_static <- function(version = NULL,
                           type = NULL,
                           spatial_hash = NULL,
                           temporal_hash = NULL,
                           as_raster = FALSE,
                           test = FALSE,
                           overwrite = FALSE) {

  base_path <- pgout_path(version = version,
                          type = type,
                          spatial_hash = spatial_hash,
                          temporal_hash = temporal_hash)

  fname <- file.path(base_path, "pg_static.parquet")

  # Return cached if available
  if (as_raster == FALSE & test == FALSE & file.exists(fname) & !overwrite) {
    return(arrow::read_parquet(fname))
  }

  # Load individual variables
  static <- pgvariables |> dplyr::filter(static)
  rasters <- lapply(static$name, load_pgvariable,
                    version = version,
                    type = type,
                    spatial_hash = spatial_hash,
                    temporal_hash = temporal_hash)
  names(rasters) <- static$name
  rasters <- rasters[!is.na(rasters)]

  # Test coverage if requested
  if (test) {
    pg_idx <- length(create_pg_indices())
    coverage_test <- dplyr::bind_rows(
      lapply(rasters, function(x) terra::freq(is.na(x))),
      .id = "variable"
    ) |>
      dplyr::filter(value == 0) |> # non-missing
      dplyr::mutate(coverage_pct = round(count / pg_idx, 4)) |>
      dplyr::select(variable, coverage_n = count, coverage_pct)
    message("# ---- Testing variable coverage ---- #")
    message(paste(capture.output(print(coverage_test, nrows = 50)), collapse = "\n"))
  }

  # Return rasters if requested
  if (as_raster) {
    return(rasters)
  }

  # Build data.table
  df <- rast_to_df(terra::rast(rasters), static = TRUE)

  # Save to cache
  arrow::write_parquet(df, fname)

  return(df)
}


#' Collect time-varying (non-static) PRIO-GRID data
#'
#' Loads all time-varying variables and returns them either as a data.table or as
#' a list of rasters. See [pgvariables] for available variables.
#'
#' @param version Character string specifying PRIOGRID version.
#' @param type Character string specifying release type (e.g., "05deg_yearly").
#' @param spatial_hash Character string with 6-character spatial hash (custom only).
#' @param temporal_hash Character string with 6-character temporal hash (custom only).
#' @param as_raster Logical. If TRUE, returns list of SpatRasters. If FALSE
#'   (default), returns data.table.
#' @param test Logical. If TRUE, returns coverage summary data.frame.
#' @param overwrite Logical. If FALSE (default) and cached file exists, returns
#'   cached data. If TRUE, rebuilds from individual variables.
#'
#' @return data.table with pgid + measurement_date as rows and variables as columns,
#'   or list of terra SpatRasters if as_raster=TRUE, or coverage test data.frame if test=TRUE
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load as data.table
#'   pg_dt <- read_pg_timevarying()
#'
#'   # Test coverage
#'   coverage <- read_pg_timevarying(test = TRUE)
#'
#'   # Load as rasters
#'   pg_rast <- read_pg_timevarying(as_raster = TRUE)
#'
#'   # Load official release
#'   pg_dt <- read_pg_timevarying(version = "3.0.0",
#'                                type = "05deg_yearly")
#' }
read_pg_timevarying <- function(version = NULL,
                                type = NULL,
                                spatial_hash = NULL,
                                temporal_hash = NULL,
                                as_raster = FALSE,
                                test = FALSE,
                                overwrite = FALSE) {

  # Determine path
  base_path <- pgout_path(version = version,
                          type = type,
                          spatial_hash = spatial_hash,
                          temporal_hash = temporal_hash)

  fname <- file.path(base_path, "pg_timevarying.parquet")

  # Return cached if available
  if (as_raster == FALSE & test == FALSE & file.exists(fname) & !overwrite) {
    return(arrow::read_parquet(fname))
  }

  # Load individual variables
  timevarying <- pgvariables |> dplyr::filter(!static)
  rasters <- lapply(timevarying$name, load_pgvariable,
                    version = version,
                    type = type,
                    spatial_hash = spatial_hash,
                    temporal_hash = temporal_hash)
  names(rasters) <- timevarying$name
  rasters <- rasters[!is.na(rasters)]

  # Test coverage if requested
  if (test) {
    pg_idx <- length(create_pg_indices())
    coverage_test <- dplyr::bind_rows(
      lapply(rasters, function(x) terra::freq(is.na(x))),
      .id = "variable"
    ) |>
      dplyr::filter(value == 0) |> # non-missing
      dplyr::mutate(
        coverage_pct = round(count / pg_idx, 4),
        measurement_date = lapply(rasters, names) |> unlist()
      ) |>
      dplyr::select(variable, measurement_date, coverage_n = count, coverage_pct)

    return(coverage_test)
  }

  # Return rasters if requested
  if (as_raster) {
    return(rasters)
  }

  # Build data.table
  timevarying_lst <- mapply(rast_to_df, rasters, names(rasters),
                            static = FALSE, SIMPLIFY = FALSE)

  # Only return dates that are within the data scope
  min_date <- do.call(min, lapply(timevarying_lst, function(x) min(x$measurement_date)))
  my_dates <- pg_dates()
  my_dates <- my_dates[my_dates >= min_date]

  df <- expand.grid(pgid = create_pg_indices(), measurement_date = my_dates)
  df <- data.table::setDT(df, key = c("pgid", "measurement_date"))

  for (sdt in timevarying_lst) {
    df <- merge(df, sdt, all.x = TRUE)
  }

  # Save to cache
  data.table::fwrite(df, paste0(tools::file_path_sans_ext(fname), ".csv.gz"),
                     compress = "gzip")
  arrow::write_parquet(df, fname)

  return(df)
}

#' Build an official PRIO-GRID release
#'
#' Sets pgoptions to specified configuration, calculates all variables to custom
#' location, then copies them to the official release folder. This ensures
#' consistency between custom and release workflows.
#'
#' @param version Character string with release version (e.g., "3.0.0")
#' @param type Character string with release type (e.g., "05deg_yearly")
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param crs Coordinate reference system
#' @param extent Named vector with xmin, xmax, ymin, ymax
#' @param temporal_resolution Temporal resolution string
#' @param start_date Start date (Date object)
#' @param end_date End date (Date object)
#'
#' @return NULL (invisibly). Called for side effects (creating release).
#' @export
#'
#' @examples
#' \dontrun{
#'   build_release(
#'     version = "3.0.0",
#'     type = "05deg_yearly",
#'     nrow = 360,
#'     ncol = 720,
#'     crs = "epsg:4326",
#'     extent = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
#'     temporal_resolution = "1 year",
#'     start_date = as.Date("1850-12-31"),
#'     end_date = as.Date("2025-08-26")
#'  )
#' }
build_release <- function(version, type,
                          nrow, ncol,
                          crs, extent,
                          temporal_resolution,
                          start_date, end_date) {

  # Set pgoptions
  pgoptions$set_nrow(nrow)
  pgoptions$set_ncol(ncol)
  pgoptions$set_crs(crs)
  pgoptions$set_extent(extent)
  pgoptions$set_temporal_resolution(temporal_resolution)
  pgoptions$set_start_date(start_date)
  pgoptions$set_end_date(end_date)

  # Calculate
  calc_pg(overwrite = TRUE)
  read_pg_static(overwrite = TRUE)
  read_pg_timevarying(overwrite = TRUE)

  # Promote to release
  from_path <- pgout_path()
  to_path <- pgout_path(version = version, type = type)

  dir.create(dirname(to_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(from_path, dirname(to_path), recursive = TRUE)
  file.rename(file.path(dirname(to_path), basename(from_path)), to_path)

  message("Release built at: ", to_path)
  invisible(NULL)
}

#' Download and initialize official PRIO-GRID release
#'
#' Downloads the official PRIO-GRID release data from the PRIO CDN and extracts
#' it to the local raw data folder. Only downloads if the file doesn't exist
#' or if overwrite=TRUE.
#'
#' @param version Character string with release version (default: "3.0.0")
#' @param type Character string with release type (default: "05deg_yearly")
#' @param overwrite Logical. If TRUE, re-downloads even if file exists.
#'
#' @return NULL (invisibly). Called for side effects (downloading data).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Download official 3.0.0 release
#'   initialize_priogrid()
#'
#'   # Force re-download
#'   initialize_priogrid(overwrite = TRUE)
#' }
initialize_priogrid <- function(version = "3.0.0",
                                type = "05deg_yearly",
                                overwrite = FALSE) {

  releases <- list(
    "3.0.0_05deg_yearly" = "https://cdn.cloud.prio.org/files/379b7254-b47c-48f3-a650-783348d0ff7e"
  )

  key <- paste(version, type, sep = "_")
  if (!key %in% names(releases)) {
    stop("Unknown release: ", version, " (", type, ")")
  }

  fname <- paste0("priogrid_", gsub("\\.", "_", version), "_", type, ".zip")
  fpath <- file.path(pgoptions$get_rawfolder(), "priogrid", fname)

  if (!file.exists(fpath) || overwrite) {
    curl::multi_download(releases[[key]], destfiles = fpath, resume = TRUE)
  }

  unzip(fpath, exdir = file.path(pgoptions$get_rawfolder(), "priogrid"))
  invisible(NULL)
}
