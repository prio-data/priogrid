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

  tif_date <- lubridate::ymd("2000-01-01") |> as.character()

  names(tif_file) <- tif_date
  return(tif_file)
}

#' Generate traveltime
#'
#' Aggregates the high resolution travel time data to PRIO-GRID level
#'
#' Returns the distribution of travel time for the given percentile.
#'
#' @param percentile 25, 50, 75, 90. Default 50.
#' @export
#'
#' @examples
#' # tt <- gen_traveltime(percentile = 75)
#'
#'
#' @references
#' \insertRef{nelsonTravelTimeMajor2008}{priogrid}
gen_traveltime <- function(percentile = 50) {
  tt <- read_traveltime()
  pg <- prio_blank_grid()

  if (!percentile %in% c(25, 50, 75, 90)) {
    stop("percentile must be one of 25, 50, 75, or 90.")
  }

  quantile_fun <- function(x) {
    return(quantile(x, probs = percentile / 100, na.rm = TRUE))
  }

  aggregated_raster <- terra::aggregate(tt, fact = terra::res(pg)/terra::res(tt), fun = quantile_fun)

  terra::ext(aggregated_raster) <- terra::ext(pg)
  return(aggregated_raster)
}


