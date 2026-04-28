# Helper operator for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Get spatial configuration hash
#'
#' Creates a 6-character MD5 hash of the current spatial options from config.
#'
#' @return Character string with 6-character hash
#' @keywords internal
get_spatial_hash <- function(config = pg_current_config()) {
  spatial_options <- list(
    nrow = config$nrow,
    ncol = config$ncol,
    crs = config$crs,
    extent = as.vector(config$extent)
  )

  hash_input <- paste(spatial_options, collapse = "_")
  digest::digest(hash_input, algo = "md5") |> substr(1, 6)
}

#' Get temporal configuration hash
#'
#' Creates a 6-character MD5 hash of the current temporal options from config.
#' Rounds end_date to avoid creating new hashes for small date changes.
#'
#' @return Character string with 6-character hash
#' @keywords internal
get_temporal_hash <- function(config = pg_current_config()) {
  rounded_end_date <- lubridate::floor_date(
    config$end_date,
    config$temporal_resolution
  )

  temporal_options <- list(
    temporal_resolution = config$temporal_resolution,
    start_date = config$start_date,
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
#' @param spatial_hash Character string with 6-character spatial hash. If NULL, computed from current config.
#' @param temporal_hash Character string with 6-character temporal hash. If NULL, computed from current config.
#'
#' @return Character string with file path
#' @export
#'
#' @examples
#' \dontrun{
#'   # Custom data with current config
#'   pgout_path()
#'
#'   # Custom data with specific hashes
#'   pgout_path(spatial_hash = "a3f2e1", temporal_hash = "9c8b7a")
#'
#'   # Official release
#'   pgout_path(version = "3.0.1", type = "05deg_yearly")
#' }
pgout_path <- function(version = NULL,
                       type = NULL,
                       spatial_hash = NULL,
                       temporal_hash = NULL,
                       config = pg_current_config()) {
  base_path <- file.path(pg_rawfolder(), "priogrid")

  if (!is.null(type)) {
    # Release mode
    if (is.null(version)) {
      stop("Release mode requires both 'version' and 'type' parameters")
    }
    return(file.path(base_path, "releases", version, type))
  }

  # Custom mode
  pkg_version <- version %||% packageVersion("priogrid")
  s_hash <- spatial_hash %||% get_spatial_hash(config)
  t_hash <- temporal_hash %||% get_temporal_hash(config)

  file.path(base_path, "custom", pkg_version, s_hash, t_hash)
}


#' Calculate PRIO-GRID variables
#'
#' Calculates PRIO-GRID variables based on current config and saves them
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
#'   r <- load_pgvariable("ne_disputed_area_share")
#'
#'   # Calculate all variables
#'   calc_pg()
#'
#'   # Recalculate existing variables
#'   calc_pg(overwrite = TRUE)
#' }
calc_pg <- function(varnames = NULL, overwrite = FALSE, config = pg_current_config()) {
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

  save_to <- pgout_path(config = config)

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
    gen_fun <- get(paste0("gen_", varname))
    r <- gen_fun(config = config)
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
#'   data location based on config.
#'
#' @return NULL (invisibly). Called for side effects (saving file).
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- gen_ne_disputed_area_share()
#'   save_pgvariable(r, "ne_disputed_area_share")
#' }
save_pgvariable <- function(rast, varname, save_to = pgout_path(), config = pg_current_config()) {
  rlang::check_installed("terra", reason = "to save PRIO-GRID variable rasters")
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
#' By default, loads from the official PRIOGRID release matching the current
#' package version. Can also load custom data generated by the user.
#'
#' @param varname Character string with the variable name
#' @param version Character string specifying PRIOGRID version (e.g., "3.0.1").
#'   If NULL, uses current package version. Ignored if loading custom data.
#' @param type Character string specifying release type (e.g., "05deg_yearly").
#'   If NULL, uses "05deg_yearly". Ignored if loading custom data.
#' @param custom Logical. If TRUE, loads from custom output directory using
#'   current config settings. Default FALSE.
#' @param spatial_hash Character string with 6-character spatial hash for custom data.
#'   Requires temporal_hash. Overrides config if provided.
#' @param temporal_hash Character string with 6-character temporal hash for custom data.
#'   Requires spatial_hash. Overrides config if provided.
#'
#' @return Terra SpatRaster object
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load from current official release (default)
#'   r <- load_pgvariable("cshapes_gwcode")
#'
#'   # Load from specific official release
#'   r <- load_pgvariable("cshapes_gwcode",
#'                        version = "3.0.1",
#'                        type = "05deg_yearly")
#'
#'   # Load from custom data using current config
#'   r <- load_pgvariable("cshapes_gwcode", custom = TRUE)
#'
#'   # Load from specific custom configuration
#'   r <- load_pgvariable("cshapes_gwcode",
#'                        custom = TRUE,
#'                        spatial_hash = "ecf4dd",
#'                        temporal_hash = "727cca")
#' }
load_pgvariable <- function(varname,
                            custom = FALSE,
                            version = NULL,
                            type = NULL,
                            spatial_hash = NULL,
                            temporal_hash = NULL,
                            config = pg_current_config()) {
  rlang::check_installed("terra", reason = "to load PRIO-GRID variable rasters")

  # Path 1: Load from custom data
  if (custom) {
    # Check if both hashes provided
    if (!is.null(spatial_hash) && !is.null(temporal_hash)) {
      if (config$verbose) {
        message("Loading custom data using specified hashes")
      }
      filepath <- file.path(
        pgout_path(spatial_hash = spatial_hash, temporal_hash = temporal_hash, config = config),
        paste0(varname, ".rds")
      )
    }
    # Only one hash provided - error
    else if (!is.null(spatial_hash) || !is.null(temporal_hash)) {
      stop("Both spatial_hash and temporal_hash must be provided together")
    }
    # No hashes - use current config
    else {
      if (config$verbose) {
        message("Loading custom data using current config settings")
      }
      filepath <- file.path(
        pgout_path(config = config),
        paste0(varname, ".rds")
      )
    }
  }

  # Path 2: Load from official release
  else {
    # Ignore hashes if provided without custom = TRUE
    if (!is.null(spatial_hash) || !is.null(temporal_hash)) {
      warning("spatial_hash and temporal_hash are ignored when custom = FALSE")
    }

    # Set defaults for official release
    if (is.null(version)) {
      version <- as.character(packageVersion("priogrid"))
    }
    if (is.null(type)) {
      type <- "05deg_yearly"
    }

    if (config$verbose) {
      message("Loading official PRIOGRID version ", version, " (", type, ")")
    }

    download_priogrid(version = version, type = type)
    filepath <- file.path(
      pgout_path(version = version, type = type, config = config),
      paste0(varname, ".rds")
    )
  }

  if (!file.exists(filepath)) {
    stop("Variable '", varname, "' not found at: ", filepath)
  }

  terra::unwrap(readRDS(filepath))
}

#' Resolve PRIO-GRID data mode and paths
#'
#' Determines whether to use release or custom mode based on provided parameters,
#' and returns resolved settings. Does not perform any file operations or downloads.
#'
#' @param version Character string specifying PRIOGRID version.
#' @param type Character string specifying release type.
#' @param spatial_hash Character string with 6-character spatial hash.
#' @param temporal_hash Character string with 6-character temporal hash.
#' @param overwrite Logical. Whether to overwrite existing files.
#'
#' @return A list with:
#'   \item{mode}{"release" or "custom"}
#'   \item{base_path}{Resolved file path}
#'   \item{version}{Resolved version (NULL if custom)}
#'   \item{type}{Resolved type (NULL if custom)}
#'   \item{spatial_hash}{Resolved spatial hash (NULL if release)}
#'   \item{temporal_hash}{Resolved temporal hash (NULL if release)}
#'   \item{overwrite}{Possibly modified overwrite flag}
#'   \item{warning}{Warning message if any, otherwise NULL}
#'
#' @keywords internal
resolve_pg_mode <- function(version = NULL,
                            type = NULL,
                            spatial_hash = NULL,
                            temporal_hash = NULL,
                            overwrite = FALSE,
                            config = pg_current_config()) {

  result <- list(
    mode = NULL,
    base_path = NULL,
    version = NULL,
    type = NULL,
    spatial_hash = NULL,
    temporal_hash = NULL,
    overwrite = overwrite,
    warning = NULL
  )

  has_release_params <- !is.null(version) || !is.null(type)
  has_custom_params <- !is.null(spatial_hash) || !is.null(temporal_hash)

  # Validate: can't mix release and custom params
  if (has_release_params && has_custom_params) {
    stop("Cannot mix release parameters (version/type) with custom parameters (spatial_hash/temporal_hash)")
  }

  # Validate: both hashes required if either provided
  if (has_custom_params && (is.null(spatial_hash) || is.null(temporal_hash))) {
    stop("Both spatial_hash and temporal_hash must be provided together")
  }

  if (has_custom_params) {
    # Custom mode with explicit hashes
    result$mode <- "custom"
    result$spatial_hash <- spatial_hash
    result$temporal_hash <- temporal_hash
    result$base_path <- pgout_path(spatial_hash = spatial_hash, temporal_hash = temporal_hash, config = config)

  } else if (has_release_params) {
    # Explicit release mode
    result$mode <- "release"
    result$version <- version %||% as.character(packageVersion("priogrid"))
    result$type <- type %||% "05deg_yearly"
    result$base_path <- pgout_path(version = result$version, type = result$type, config = config)

    if (overwrite) {
      result$warning <- paste0(
        "overwrite = TRUE is ignored for official releases. ",
        "Use build_release() to create releases."
      )
      result$overwrite <- FALSE
    }

  } else if (overwrite) {
    # No params but overwrite = TRUE → custom mode with current config
    result$mode <- "custom"
    result$base_path <- pgout_path(config = config)

  } else {
    # No params, no overwrite → default to official release
    result$mode <- "release"
    result$version <- as.character(packageVersion("priogrid"))
    result$type <- "05deg_yearly"
    result$base_path <- pgout_path(version = result$version, type = result$type, config = config)
  }

  result
}

#' Collect static (non-time-varying) PRIO-GRID data
#'
#' Loads all static variables and returns them either as a data.table or as
#' a list of rasters. See [pgvariables] for available variables. If an official release
#' is specified (or version, type and hashes are NULL), then this is downloaded if this
#' is not already done.
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
#'   pg_dt <- read_pg_static(version = "3.0.1",
#'                           type = "05deg_yearly")
#' }
read_pg_static <- function(version = NULL,
                           type = NULL,
                           spatial_hash = NULL,
                           temporal_hash = NULL,
                           as_raster = FALSE,
                           test = FALSE,
                           overwrite = FALSE,
                           config = pg_current_config()) {

  cfg <- resolve_pg_mode(version, type, spatial_hash, temporal_hash, overwrite, config)

  if (!is.null(cfg$warning)) {
    warning(cfg$warning)
  }

  if (cfg$mode == "release") {
    download_priogrid(version = cfg$version, type = cfg$type)
  }

  fname <- file.path(cfg$base_path, "pg_static.parquet")

  # Return cached if available (lightweight path — no terra needed)
  if (!as_raster && !test && file.exists(fname) && !cfg$overwrite) {
    return(nanoparquet::read_parquet(fname))
  }

  # From here on, terra is required
  rlang::check_installed("terra", reason = "to build PRIO-GRID data from individual variables")

  # Load individual variables
  static <- pgvariables |> dplyr::filter(static)

  if (cfg$mode == "release") {
    rasters <- lapply(static$name, load_pgvariable,
                      version = cfg$version, type = cfg$type, config = config)
  } else {
    rasters <- lapply(static$name, load_pgvariable,
                      custom = TRUE,
                      spatial_hash = cfg$spatial_hash,
                      temporal_hash = cfg$temporal_hash,
                      config = config)
  }

  names(rasters) <- static$name
  rasters <- rasters[!is.na(rasters)]

  # Test coverage if requested
  if (test) {
    pg_idx <- length(create_pg_indices(config))
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
  df <- rast_to_df(terra::rast(rasters), static = TRUE, config = config)

  # Save to cache
  rlang::check_installed("arrow", reason = "to write parquet files")
  arrow::write_parquet(df, fname)

  return(df)
}


#' Collect time-varying (non-static) PRIO-GRID data
#'
#' Loads all time-varying variables and returns them either as a data.table or as
#' a list of rasters. See [pgvariables] for available variables. If an official release
#' is specified (or version, type and hashes are NULL), then this is downloaded if this
#' is not already done.
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
#'   pg_dt <- read_pg_timevarying(version = "3.0.1",
#'                                type = "05deg_yearly")
#' }
read_pg_timevarying <- function(version = NULL,
                                type = NULL,
                                spatial_hash = NULL,
                                temporal_hash = NULL,
                                as_raster = FALSE,
                                test = FALSE,
                                overwrite = FALSE,
                                config = pg_current_config()) {

  cfg <- resolve_pg_mode(version, type, spatial_hash, temporal_hash, overwrite, config)

  if (!is.null(cfg$warning)) {
    warning(cfg$warning)
  }

  if (cfg$mode == "release") {
    download_priogrid(version = cfg$version, type = cfg$type)
  }

  fname <- file.path(cfg$base_path, "pg_timevarying.parquet")

  # Return cached if available (lightweight path — no terra needed)
  if (!as_raster && !test && file.exists(fname) && !cfg$overwrite) {
    return(nanoparquet::read_parquet(fname))
  }

  # From here on, terra is required
  rlang::check_installed("terra", reason = "to build PRIO-GRID data from individual variables")

  # Load individual variables
  timevarying <- pgvariables |> dplyr::filter(!static)

  if (cfg$mode == "release") {
    rasters <- lapply(timevarying$name, load_pgvariable,
                      version = cfg$version, type = cfg$type, config = config)
  } else {
    rasters <- lapply(timevarying$name, load_pgvariable,
                      custom = TRUE,
                      spatial_hash = cfg$spatial_hash,
                      temporal_hash = cfg$temporal_hash,
                      config = config)
  }

  names(rasters) <- timevarying$name
  rasters <- rasters[!is.na(rasters)]

  # Test coverage if requested
  if (test) {
    pg_idx <- length(create_pg_indices(config))
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
                            MoreArgs = list(static = FALSE, config = config),
                            SIMPLIFY = FALSE)

  # Only return dates that are within the data scope
  min_date <- do.call(min, lapply(timevarying_lst, function(x) min(x$measurement_date)))
  my_dates <- pg_dates(config)
  my_dates <- my_dates[my_dates >= min_date]

  df <- expand.grid(pgid = create_pg_indices(config), measurement_date = my_dates)
  df <- data.table::setDT(df, key = c("pgid", "measurement_date"))

  for (sdt in timevarying_lst) {
    df <- merge(df, sdt, all.x = TRUE)
  }

  # Save to cache
  data.table::fwrite(df, paste0(tools::file_path_sans_ext(fname), ".csv.gz"),
                     compress = "gzip")
  rlang::check_installed("arrow", reason = "to write parquet files")
  arrow::write_parquet(df, fname)

  return(df)
}

#' Build an official PRIO-GRID release
#'
#' Sets config to specified configuration, calculates all variables to custom
#' location, then copies them to the official release folder. This ensures
#' consistency between custom and release workflows.
#'
#' @param version Character string with release version (e.g., "3.0.1")
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
#'     version = "3.0.1",
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

  # Construct config from explicit arguments
  config <- pg_config(
    nrow = nrow, ncol = ncol, crs = crs, extent = extent,
    temporal_resolution = temporal_resolution,
    start_date = start_date, end_date = end_date
  )

  # Calculate
  calc_pg(overwrite = TRUE, config = config)
  read_pg_static(overwrite = TRUE, config = config)
  read_pg_timevarying(overwrite = TRUE, config = config)

  # Promote to release
  from_path <- pgout_path(config = config)
  to_path <- pgout_path(version = version, type = type, config = config)

  dir.create(dirname(to_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(from_path, dirname(to_path), recursive = TRUE)
  file.rename(file.path(dirname(to_path), basename(from_path)), to_path)

  current_wd <- getwd()
  setwd(file.path(pg_rawfolder(), "priogrid"))
  safe_version <- stringr::str_replace_all(version, "\\.", "_")
  zip(zipfile = paste0(paste("priogrid", safe_version, type, sep = "_"), ".zip"),
      files = list.files(file.path("releases", version, type), full.names = TRUE))
  setwd(current_wd)

  message("Release built at: ", to_path)
  invisible(NULL)
}

#' Download and initialize official PRIO-GRID release
#'
#' Downloads the official PRIO-GRID release data from the PRIO CDN and extracts
#' it to the local raw data folder. Only downloads if the file doesn't exist
#' or if overwrite=TRUE.
#'
#' @param version Character string with release version
#' @param type Character string with release type (default: "05deg_yearly")
#' @param overwrite Logical. If TRUE, re-downloads even if file exists.
#'
#' @return NULL (invisibly). Called for side effects (downloading data).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Download latest official release
#'   download_priogrid()
#'
#'   # Download specific release
#'   download_priogrid(version = "3.0.1", type = "05deg_yearly")
#'
#'   # List releases
#'   download_priogrid(list_releases = TRUE)
#' }
download_priogrid <- function(version = NULL,
                              type = "05deg_yearly",
                              overwrite = FALSE,
                              list_releases = FALSE) {

  if(is.null(version)){
    version <- packageVersion("priogrid")
  }

  releases <- list(
    "3.0.0_05deg_yearly" = "https://cdn.cloud.prio.org/files/379b7254-b47c-48f3-a650-783348d0ff7e",
    "3.0.1_05deg_yearly" = "https://cdn.cloud.prio.org/files/1c76a606-8efa-4dc0-a938-301c4e9331e6"
  )

  if(list_releases == TRUE){
   df <- stringr::str_split(names(releases), "_", n = 2, simplify = T) |> as.data.frame()
   df$url <- unlist(releases)
   names(df) <- c("version", "type", "url")
   df <- dplyr::tibble(df)
   return(print(df))
  }

  key <- paste(version, type, sep = "_")
  if (!key %in% names(releases)) {
    stop("Unknown release: ", version, " (", type, ")")
  }

  fname <- paste0("priogrid_", gsub("\\.", "_", version), "_", type, ".zip")
  fpath <- file.path(pg_rawfolder(), "priogrid", fname)

  if (!file.exists(fpath) || overwrite) {
    curl::multi_download(releases[[key]], destfiles = fpath, resume = TRUE)
  }

  suppressWarnings(
    unzip(fpath, exdir = file.path(pg_rawfolder(), "priogrid"), overwrite = overwrite)
  )

  invisible(NULL)
}
