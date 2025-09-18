#’ Reads the Li Nighttime data
#’
#’
#’
#’ @return an object of class sf
#’ @export
#’
#’ @references
#’ \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
read_linight <- function(overwrite_files = FALSE){

  zip_file <- get_pgfile(source_name="Li Nighttime",
                         source_version="v8",
                         id="24d76a3b-927e-42ad-b8a5-2e7443e6a275")


  suppressWarnings(unzip(zipfile = zip_file, overwrite = overwrite_files, exdir = dirname(zip_file)))

  allfiles <- list.files(dirname(zip_file),pattern = "^Harmonized", full.names = TRUE)

  # Extents vary depending on the source, and is often marginally larger than the world.
  #extents <- lapply(allfiles, function(x) terra::ext(terra::rast(x)))
  #allfiles[!sapply(extents, function(x) x == terra::ext(c(-180, 180, -90, 90)))]

  fixed_files <- list.files(dirname(zip_file),pattern = "^fixed", full.names = TRUE)
  if(overwrite_files){
    # Re-calculate from source file
    file.remove(fixed_files)
    fixed_files <- list.files(dirname(zip_file),pattern = "^fixed", full.names = TRUE)
  }

  fixed_files <- fixed_files[file.info(fixed_files)$size > 3e7] # can occur if resampling is interrupted

  files_to_fix <- allfiles[!basename(allfiles) %in% stringr::str_remove(basename(fixed_files), "^fixed_")]

  if(length(files_to_fix) > 0){
    message("Harmonizing extent of Li Nighttime rasters. Next time you run the function, this will not be required")

    # Extent of many tifs are wrong, use template
    template <- terra::rast(vals = NA,
                             nrows = 21600, # Note that this is 1 cell less than original data
                             ncols = 43200, # Note that this is 1 cell less than original data
                             extent = terra::ext(c(-180, 180, -90, 90)),
                             crs = "EPSG:4326"
    )

    #template <- terra::rast(x = files_to_fix[1])
    #template <- terra::extend(template, terra::ext(c(-180, 180, -90, 90)))

    n <- length(files_to_fix)
    pb <- txtProgressBar(min = 0, max = n, style = 3)

    for(i in 1:n){
      setTxtProgressBar(pb, i)
      rsub <- terra::rast(x = files_to_fix[i])
      fname <- paste0("fixed_", basename(files_to_fix[i]))
      res <- terra::resample(rsub, template, method = "near", threads = T, overwrite = TRUE, progress = FALSE, filename = file.path(dirname(zip_file), fname))
    }
    close(pb)
  }

  fixed_files <- list.files(dirname(zip_file),pattern = "^fixed", full.names = TRUE)
  r <- terra::rast(fixed_files)

  pgmonth <- pg_dates()[1] |> lubridate::month()
  pgday <- pg_dates()[1] |> lubridate::day()
  yearnames <- readr::parse_number(basename(allfiles))
  yearnames <- as.Date(paste(yearnames, pgmonth, pgday, sep = "-"))
  names(r) <- yearnames
  return(r)
}

#' Generate Li Nighttime Light
#'
#' This aggregates the high-resolution Li Nighttime Light to PRIO-GRID level across all years (1992-2021).
#'
#' A slight nearest neighbor resampling was applied to get the exact PRIO-GRID extent.This is done in [read_linight()].
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_linight_mean()
#' @references
#' \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
gen_linight_mean <- function(){
  r <- read_linight()

  res <- robust_transformation(r, agg_fun = "mean")

  return(res)
}

