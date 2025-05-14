

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

  ext(aggregated_raster) <- ext(pg)
  return(aggregated_raster)
}

