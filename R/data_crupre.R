#' Read and Process CRU TS Precipitation Data
#'
#' This function reads CRU TS (Climate Research Unit Time Series) v4.09
#' precipitation data files from local storage, unzips them if necessary,
#' loads them as raster stacks (\code{SpatRaster} objects), and filters the data
#' to match PRIO-GRID temporal coverage. The function also corrects broken or missing
#' time stamps by reconstructing a monthly sequence starting from January 1901,
#' ensuring consistent temporal alignment with PRIO-GRID.
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
#'     \item \strong{Units}: Millimeters of precipitation
#'   }
#'
#' @examples
#' \dontrun{
#' # Load monthly precipitation data
#' pre_data <- read_cru_pre()
#' }
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval functionality,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal boundaries
#'
#' @export
#' @references
#' \insertRef{harrisVersion4CRU2020}{priogrid}
read_cru_pre <- function() {

  cru_file <- get_pgfile(
    source_name = "CRU Climate pre",
    source_version = "v4.09",
    id = "00575260-ad1c-4e87-a575-3922bc151f50")

  gz_file <- cru_file[1]
  nc_file <- tools::file_path_sans_ext(gz_file)

  if (!file.exists(nc_file)) {
    R.utils::gunzip(gz_file, destname = nc_file, remove = FALSE, overwrite = TRUE)
  }

  r <- terra::rast(nc_file)

  n <- terra::nlyr(r)
  start_date <- as.Date("1901-01-16")
  clean_time <- seq.Date(start_date, by = "month", length.out = n)

  terra::time(r) <- clean_time
  names(r) <- clean_time

  pg_period <- lubridate::interval(
    pg_date_intervals() |> lubridate::int_start() |> min(),
    pg_date_intervals() |> lubridate::int_end()   |> max()
  )

  terra::subset(r, which(terra::time(r) %within% pg_period))
}

#' Generate PRIO-GRID Compatible CRU Precipitation Data
#'
#' This function processes CRU TS v4.09 precipitation (PRE)
#' data by aggregating it temporally and spatially to match PRIO-GRID
#' specifications.
#'
#' @return A \code{SpatRaster} object with PRIO-GRID spatio-temporal resolution.
#'
#' @examples
#' \dontrun{
#' r <- gen_cru_pre()
#' }
#'
#' @seealso
#' \code{\link{read_cru_pre}}, \code{\link{pg_date_intervals}},
#' \code{\link{robust_transformation}}, \code{\link{get_pgfile}}
#'
#' @export
gen_cru_pre <- function(){
  r <- read_cru_pre()

  cru_time_interval <- lubridate::interval(min(terra::time(r)), max(terra::time(r)))

  pg_intervals <- pg_date_intervals()[pg_date_intervals() %within% cru_time_interval]

  res <- list()
  time_groups <- lapply(pg_intervals, function(x) which(terra::time(r) %within% x))

  for (i in seq_along(time_groups)) {
    rt <- terra::subset(r, time_groups[[i]])
    rt <- terra::app(rt, fun = sum, na.rm = TRUE)
    res[[i]] <- rt
  }

  res <- terra::rast(res)
  names(res) <- lubridate::int_end(pg_intervals)


  res <- robust_transformation(res, agg_fun = "mean")
  return(res)
}
