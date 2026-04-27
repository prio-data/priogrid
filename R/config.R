# Internal environment for session-scoped state
.pg_state <- new.env(parent = emptyenv())

# Persistent cache for rawfolder only
.pg_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))

#' Create a PRIO-GRID configuration object
#'
#' Constructs a configuration object with analysis parameters for PRIO-GRID.
#' Config objects are session-scoped and not persisted across sessions. Use
#' [pg_set_rawfolder()] for persistent machine-level settings.
#'
#' @param nrow Integer. Number of rows in the grid.
#' @param ncol Integer. Number of columns in the grid.
#' @param crs Character. Coordinate reference system string.
#' @param extent Numeric vector. Spatial extent c(xmin, xmax, ymin, ymax).
#' @param temporal_resolution Character. Increment of temporal sequence, see [base::seq.Date].
#' @param start_date Date. The start date for temporal coverage. The month and day define
#'   the measurement date within the temporal resolution.
#' @param end_date Date or "today". The end date for temporal coverage.
#' @param verbose Logical. If TRUE, print informative messages.
#' @param automatic_download Logical. If TRUE, automatically download needed files.
#'
#' @return A `pg_config` object (a named list with class `"pg_config"`).
#' @export
#'
#' @examples
#' cfg <- pg_config(nrow = 180, ncol = 360)
#' cfg$nrow
pg_config <- function(nrow = 360L,
                      ncol = 720L,
                      crs = "epsg:4326",
                      extent = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
                      temporal_resolution = "1 year",
                      start_date = as.Date("1850-12-31"),
                      end_date = Sys.Date(),
                      verbose = TRUE,
                      automatic_download = TRUE) {

  if (!is.numeric(extent) || length(extent) != 4) stop("extent must be a numeric vector of length 4")

  cfg <- list(
    nrow = as.integer(nrow),
    ncol = as.integer(ncol),
    crs = as.character(crs),
    extent = setNames(as.numeric(extent), c("xmin", "xmax", "ymin", "ymax")),
    temporal_resolution = as.character(temporal_resolution),
    start_date = as.Date(start_date),
    end_date = if (identical(end_date, "today")) Sys.Date() else as.Date(end_date),
    verbose = as.logical(verbose),
    automatic_download = as.logical(automatic_download)
  )

  validate_pg_config(cfg)
  class(cfg) <- "pg_config"
  cfg
}

#' Get the current session configuration
#'
#' Returns the current session-scoped PRIO-GRID config. If none has been set,
#' initializes with defaults from [pg_config()].
#'
#' @return A `pg_config` object.
#' @export
pg_current_config <- function() {
  if (is.null(.pg_state$config)) {
    .pg_state$config <- pg_config()
  }
  .pg_state$config
}

#' Update the current session configuration
#'
#' Modifies named fields of the current session config. Only the fields
#' you specify are changed; others are preserved.
#'
#' @param ... Named arguments matching [pg_config()] parameters.
#' @return The updated `pg_config` object (invisibly).
#' @export
#'
#' @examples
#' pg_set_config(nrow = 180, ncol = 360)
#' pg_current_config()$nrow
pg_set_config <- function(...) {
  current <- pg_current_config()
  updates <- list(...)

  valid_fields <- c("nrow", "ncol", "crs", "extent", "temporal_resolution",
                    "start_date", "end_date", "verbose", "automatic_download")
  invalid <- setdiff(names(updates), valid_fields)
  if (length(invalid) > 0) {
    stop("Unknown config fields: ", paste(invalid, collapse = ", "),
         ". Valid fields: ", paste(valid_fields, collapse = ", "))
  }

  for (nm in names(updates)) {
    current[[nm]] <- updates[[nm]]
  }

  # Re-construct to apply type coercion and validation
  .pg_state$config <- do.call(pg_config, current)
  invisible(.pg_state$config)
}

#' Reset session configuration to defaults
#'
#' Resets the current session config to the defaults from [pg_config()].
#'
#' @return The default `pg_config` object (invisibly).
#' @export
pg_reset_config <- function() {
  .pg_state$config <- pg_config()
  invisible(.pg_state$config)
}

#' Print a pg_config object
#'
#' @param x A `pg_config` object.
#' @param ... Ignored.
#' @return `x` invisibly.
#' @export
print.pg_config <- function(x, ...) {
  cat("PRIO-GRID config:\n")
  cat("  nrow:", x$nrow, "\n")
  cat("  ncol:", x$ncol, "\n")
  cat("  crs:", x$crs, "\n")
  cat("  extent:", x$extent, "\n")
  cat("  temporal_resolution:", x$temporal_resolution, "\n")
  cat("  start_date:", as.character(x$start_date), "\n")
  cat("  end_date:", as.character(x$end_date), "\n")
  cat("  verbose:", x$verbose, "\n")
  cat("  automatic_download:", x$automatic_download, "\n")
  invisible(x)
}

# ---- Persistent machine settings (rawfolder only) ----

#' Get the raw data folder path
#'
#' Returns the persistent rawfolder path. Errors with a helpful message if unset.
#'
#' @return Character string with the raw data folder path.
#' @export
pg_rawfolder <- function() {
  val <- .pg_cache$get("rawfolder")
  if (cachem::is.key_missing(val)) {
    stop("Raw data folder is not set. Use pg_set_rawfolder(path) to set it.")
  }
  val
}

#' Set the raw data folder path
#'
#' Persists the raw data folder path across sessions. Creates `tmp` and
#' `priogrid` subdirectories if they don't exist.
#'
#' @param path Character string. Path to the folder for raw data storage.
#' @return The normalized path (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' pg_set_rawfolder("/data/priogrid_raw")
#' pg_rawfolder()
#' }
pg_set_rawfolder <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(file.path(path, "tmp"))) {
    dir.create(file.path(path, "tmp"), recursive = TRUE)
  }
  if (!dir.exists(file.path(path, "priogrid"))) {
    dir.create(file.path(path, "priogrid"), recursive = TRUE)
  }
  .pg_cache$set("rawfolder", path)
  invisible(path)
}

# ---- Internal helpers ----

#' Validate a pg_config object
#' @param cfg A list with config fields.
#' @return NULL (invisibly). Stops with an error on invalid input.
#' @keywords internal
validate_pg_config <- function(cfg) {
  if (!is.integer(cfg$nrow) || cfg$nrow <= 0) stop("nrow must be a positive integer")
  if (!is.integer(cfg$ncol) || cfg$ncol <= 0) stop("ncol must be a positive integer")
  if (!is.character(cfg$crs) || length(cfg$crs) != 1) stop("crs must be a single character string")
  if (!is.numeric(cfg$extent) || length(cfg$extent) != 4) stop("extent must be a numeric vector of length 4")
  if (!is.character(cfg$temporal_resolution)) stop("temporal_resolution must be a character string")
  if (!inherits(cfg$start_date, "Date")) stop("start_date must be a Date")
  if (!inherits(cfg$end_date, "Date")) stop("end_date must be a Date")
  if (!is.logical(cfg$verbose) || is.na(cfg$verbose)) stop("verbose must be logical")
  if (!is.logical(cfg$automatic_download) || is.na(cfg$automatic_download)) stop("automatic_download must be logical")
  invisible(NULL)
}

#' Configure terra memory settings based on config
#'
#' Estimates memory usage and sets `terra::terraOptions(todisk = ...)` accordingly.
#' Only runs if terra is installed. Called internally from terra-using functions.
#'
#' @param config A `pg_config` object.
#' @keywords internal
pg_configure_terra_memory <- function(config = pg_current_config()) {
  if (!requireNamespace("terra", quietly = TRUE)) return(invisible())

  grid_cells <- config$nrow * config$ncol
  n_layers <- length(pg_dates(config))
  estimated_gb <- (grid_cells * n_layers * 8) / 1e9

  if (estimated_gb > 4) {
    terra::terraOptions(todisk = TRUE)

    rawfolder <- tryCatch(pg_rawfolder(), error = function(e) NULL)
    if (!is.null(rawfolder)) {
      temp_dir <- file.path(rawfolder, "tmp")
      if (!dir.exists(temp_dir)) {
        dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      }
      terra::terraOptions(tempdir = temp_dir)
    }

    if (config$verbose) {
      message(sprintf(
        "%s cells x %d layers ~ %.1f GB. Enabled disk-based processing.",
        format(grid_cells, big.mark = ","), n_layers, estimated_gb
      ))
    }
  } else {
    terra::terraOptions(todisk = FALSE)
  }
}
