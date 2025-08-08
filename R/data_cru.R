#' Read and Process CRU TS Near Surface Temperature Data
#'
#' This function reads CRU TS (Climate Research Unit Time Series) v4.09
#' near surface temperature climate data files from local storage, unzips them if necessary,
#' loads them as raster stacks, and filters the data to match PRIO-GRID temporal coverage. The function
#' also support reading supporting statistics.
#'
#' @param variable A character string specifying the climate variable to extract
#'   from the raster stack. Must match the prefix of variable names in the CRU
#'   dataset. Available options include "tmp" (temperature), "stn" (station count),
#'   "mae" (mean absolute error), and "maea".
#'   Default is "tmp".
#'
#' @return A \code{SpatRaster} object (from the \pkg{terra} package) with the
#'   following characteristics:
#'   \itemize{
#'     \item \strong{Spatial Resolution}: 0.5° x 0.5° global grid
#'     \item \strong{Spatial Extent}: Global coverage (-180° to 180° longitude,
#'           -90° to 90° latitude)
#'     \item \strong{Coordinate System}: WGS84 geographic (lon/lat)
#'     \item \strong{Temporal Coverage}: Filtered to match PRIO-GRID date intervals
#'     \item \strong{Layer Names}: Set to corresponding timestamps
#'     \item \strong{Units}: Variable-specific (e.g., degrees Celsius for temperature)
#'   }
#'
#' @examples
#' \dontrun{
#' # Load temperature data (default)
#' temp_data <- read_cru()
#'
#' # Load station count data
#' station_data <- read_cru(variable = "stn")
#'
#' # Load mean absolute error data
#' error_data <- read_cru(variable = "mae")
#'
#' }
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval functionality,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal boundaries
#'
#' @export
#' @references
#' \insertRef{harrisVersion4CRU2020}{priogrid}
read_cru_tmp <- function(variable = "tmp") {

  # Step 1: Get file paths to all CRU gzipped NetCDFs
  cru_files <- get_pgfile(
    source_name = "CRU Climate",
    source_version = "v4.09",
    id = "9bba83b0-eca9-4f05-b1df-6aeaed55a9fa"
  )

  # Step 2: Process each file: unzip files
  for (i in seq_along(cru_files)) {
    gz_file <- cru_files[i]
    nc_file <- tools::file_path_sans_ext(gz_file)

    # Unzip only if needed
    if (!file.exists(nc_file)) {
      R.utils::gunzip(gz_file, destname = nc_file, remove = FALSE, overwrite = TRUE)
    }
  }

  raster_stack <- terra::rast(tools::file_path_sans_ext(cru_files))
  variable_idx <- grepl(paste0("^", variable), names(raster_stack))
  raster_stack <- raster_stack[[variable_idx]]

  # Set names the same as the time
  names(raster_stack) <- terra::time(raster_stack)

  # Subset to time periods in PRIO-GRID
  pg_period <- lubridate::interval(
    pg_date_intervals() |> lubridate::int_start() |> min(),
    pg_date_intervals() |> lubridate::int_end() |> max()
  )
  raster_stack <- terra::subset(raster_stack, which(terra::time(raster_stack) %within% pg_period))

  return(raster_stack)
}

#' Generate PRIO-GRID Compatible CRU Near Surface Temperature Data
#'
#' This function processes CRU TS (Climate Research Unit Time Series) v4.09
#' near surface temperature data by aggregating it both temporally and spatially
#' to match PRIO-GRID specifications. The function takes monthly CRU temperature
#' data and transforms it to the temporal resolution defined by PRIO-GRID date
#' intervals (which may be quarterly, yearly, or other intervals), while also
#' performing spatial aggregation to the PRIO-GRID resolution.
#'
#' This takes the CRU TS dataset and aggregates it in time and space to
#' PRIO-GRID specifications.
#'
#' @return A \code{SpatRaster} object (from the \pkg{terra} package) with spatio-temporal
#' resolution as defined in PRIO-GRID.
#'
#' @examples
#' \dontrun{
#' r <- gen_cru_tmp()
#' }
#'
#' @seealso
#' \code{\link{read_cru_tmp}} for reading raw CRU temperature data,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal boundaries,
#' \code{\link{robust_transformation}} for spatial aggregation,
#' \code{\link{get_pgfile}} for file management
#'
#' @export
#' @references
#' \insertRef{harrisVersion4CRU2020}{priogrid}
gen_cru_tmp <- function(){
  r <- read_cru_tmp()

  cru_time_interval <- lubridate::interval(terra::time(r) |> min(),
                                           terra::time(r) |> max())

  pg_intervals <- pg_date_intervals()[pg_date_intervals() %within% cru_time_interval]

  res <- list()
  # CRU data comes in monthly resolution. Aggregate to lower resolution if PRIO-GRID is lower (e.g,. quarterly, yearly)
  time_groups <- lapply(pg_intervals, function(x) which(terra::time(r) %within% x))
  i <- 1
  for(tgroup in time_groups){
    rt <- terra::subset(r, tgroup)
    rt <- terra::mean(rt)
    res[[i]] <- rt
    i <- i + 1
  }
  res <- terra::rast(res)
  names(res) <- lubridate::int_end(pg_intervals)

  res <- robust_transformation(res, agg_fun = "mean")

  return(res)
}
