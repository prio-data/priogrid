#' Read Estimated Travel Time to Major Cities (Year 2000)
#'
#' Downloads and reads the global raster dataset of estimated travel time to
#' major cities for the year 2000, as developed by Nelson (2008). The dataset
#' represents travel time in minutes to the nearest major city using multimodal
#' transport networks.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Locates the local travel time dataset using \code{\link{get_pgfile}}
#'   \item Extracts the zip archive containing the raster files
#'   \item Imports the main \code{.tif} file as a \code{SpatRaster} object
#'         using the \code{terra} package
#'   \item Sets the layer name to \code{"travel_time"} for clarity
#' }
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item The dataset provides estimates based on the transportation network circa 2000
#'   \item Raster extraction is cached locally after the first download
#'   \item Large raster files may take time and memory to process
#' }
#'
#' @examples
#' \dontrun{
#' # Load estimated travel time raster
#' travel_time_raster <- read_traveltime()
#'
#' # Inspect raster properties
#' travel_time_raster
#'
#' # Plot global travel time
#' terra::plot(travel_time_raster, main = "Estimated Travel Time to Major Cities (2000)")
#'
#' # Extract travel time for a specific region
#' africa_extent <- terra::ext(-20, 50, -35, 37)
#' africa_travel <- terra::crop(travel_time_raster, africa_extent)
#' terra::plot(africa_travel, main = "Travel Time in Africa")
#' }
#'
#' @export
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
read_traveltime <- function() {
  zip_file <- get_pgfile(source_name = "Estimated Travel Time",
                  source_version = "2000",
                  id = "9aa052f6-4d04-4ed1-9eed-e47e08828d38")

  unzip_to <- file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)))
  unzip(zip_file, exdir = unzip_to)
  tif_file <- terra::rast(file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)), "acc_50k.tif"))

  names(tif_file) <- "travel_time"
  return(tif_file)
}

#' Generate travel time to nearest major city
#'
#' Aggregates the high-resolution global travel time raster to the PRIO-GRID
#' structure. Users can specify the aggregation function to summarize travel
#' time values within each PRIO-GRID cell.
#'
#' Supported aggregation functions include:
#' \code{"mean"}, \code{"max"}, \code{"min"}, \code{"median"}, \code{"sum"},
#' \code{"modal"}, \code{"any"}, \code{"all"}, \code{"prod"}, \code{"which.min"},
#' \code{"which.max"}, \code{"table"}, \code{"sd"} (sample standard deviation),
#' and \code{"std"} (population standard deviation). Users may also provide a
#' custom function.
#'
#' @param aggregation_function Function or character string specifying the
#'   aggregation method (see details above)
#'
#' @return A \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Aggregate global travel time using median
#' travel_time_median <- calc_traveltime(aggregation_function = "median")
#' terra::plot(travel_time_median, main = "PRIO-GRID Median Travel Time")
#'
#' # Aggregate using maximum travel time
#' travel_time_max <- calc_traveltime(aggregation_function = "max")
#' terra::plot(travel_time_max, main = "PRIO-GRID Maximum Travel Time")
#' }
#'
#' @export
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
calc_traveltime <- function(aggregation_function) {
  tt <- read_traveltime()
  r <- robust_transformation(tt, agg_fun = aggregation_function, na.rm = T)
  return(r)
}

#' Generate the minimum travel time to nearest major city
#'
#' Aggregates the high-resolution travel time raster to PRIO-GRID cells and
#' computes the **minimum** travel time within each cell. This is useful for
#' identifying the fastest travel access to major cities within a PRIO-GRID cell.
#'
#' @details
#' This function is a convenience wrapper around \code{\link{calc_traveltime}}
#' using \code{aggregation_function = "min"}. It reads the global travel time
#' raster and summarizes each PRIO-GRID cell to the minimum travel time value.
#'
#' @return A single-layer \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Generate minimum travel time raster
#' r <- gen_traveltime_min()
#' terra::plot(r, main = "Minimum Travel Time to Nearest Major City")
#'
#' # Extract values for a specific region (e.g., West Africa)
#' africa_extent <- terra::ext(-20, 20, 0, 20)
#' r_africa <- terra::crop(r, africa_extent)
#' terra::plot(r_africa, main = "Minimum Travel Time in West Africa")
#' }
#'
#' @export
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
gen_traveltime_min <- function(){
  r <- calc_traveltime(aggregation_function = "min")
  names(r) <- "traveltime_min"
  r
}

#' Generate the mean travel time to nearest major city
#'
#' Aggregates the high-resolution travel time raster to PRIO-GRID cells and
#' computes the **mean** travel time within each cell. This is useful for
#' identifying the average travel access to major cities within a PRIO-GRID cell.
#'
#' @details
#' This function is a convenience wrapper around \code{\link{calc_traveltime}}
#' using \code{aggregation_function = "mean"}. It reads the global travel time
#' raster and summarizes each PRIO-GRID cell to the mean travel time value.
#'
#' @return A single-layer \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Generate mean travel time raster
#' r <- gen_traveltime_mean()
#' terra::plot(r, main = "Mean Travel Time to Nearest Major City")
#'
#' # Extract values for a specific region (e.g., West Africa)
#' africa_extent <- terra::ext(-20, 20, 0, 20)
#' r_africa <- terra::crop(r, africa_extent)
#' terra::plot(r_africa, main = "Mean Travel Time in West Africa")
#' }
#'
#' @export
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
gen_traveltime_mean <- function(){
  r <- calc_traveltime(aggregation_function = "mean")
  names(r) <- "traveltime_mean"
  r
}
