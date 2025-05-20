#' Reads the Estimated Travel Time to Major Cities version year 2000 data
#' Unzips the folder and reads import the .tif file as SpatRaster
#' Adds date
#'
#' @return an object of class SpatRast
#' @export
#'
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
#' Aggregates the high resolution travel time data to PRIO-GRID level
#'
#' @param aggregation_function function used to aggregate values. Either an actual function,
#' or for the following, their name: "mean", "max", "min", "median", "sum", "modal", "any",
#' "all", "prod", "which.min", "which.max", "table", "sd" (sample standard deviation) and
#' "std" (population standard deviation)
#' @export
#'
#' @examples
#' # r <- calc_traveltime(aggregation_function = "median")
#'
#'
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
calc_traveltime <- function(aggregation_function) {
  tt <- read_traveltime()
  r <- robust_transformation(tt, agg_fun = aggregation_function, na.rm = T)
  return(r)
}

#' Generate the minimum travel time to nearest major city
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_traveltime_min()
#'
#'
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
gen_traveltime_min <- function(){
  r <- calc_traveltime(aggregation_function = "min")
  names(r) <- "traveltime_min"
  r
}

#' Generate the average (mean) travel time to nearest major city
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_traveltime_mean()
#'
#'
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
gen_traveltime_mean <- function(){
  r <- calc_traveltime(aggregation_function = "mean")
  names(r) <- "traveltime_mean"
  r
}
