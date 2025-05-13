ghsl_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))

#' Reads the GHSL GHS Population Grid data
#'
#'
#' @return an object of class sf
#' @export
#' @references
#' \insertRef{schiavinaGHSPOPR2023AGHS2023}{priogrid}
read_ghsl_population_grid <- function(){
  zip_files <- get_pgfile(source_name = "GHSL GHS Population Grid",
                  source_version = "R2023",
                  id = "ae6a7612-4bef-452f-acd6-d2212cf9a7c5")

  # unzip(zip_files[1], list = T)

  tif_files <- basename(zip_files) |> tools::file_path_sans_ext() |> paste0(".tif")

  for(i in 1:length(zip_files)){
    if(!file.exists(file.path(dirname(zip_files[i]), tif_files[i]))){
      unzip(zip_files[i], files = tif_files[i], exdir = dirname(zip_files[i]))
    }
  }

  r <- terra::rast(file.path(dirname(zip_files[i]), tif_files[1]))
  for(i in 2:length(tif_files)){
    terra::add(r) <- terra::rast(file.path(dirname(zip_files[i]), tif_files[i]))
  }

  pgmonth <- pg_dates()[1] |> lubridate::month()
  pgday <- pg_dates()[1] |> lubridate::day()
  tif_dates <- stringr::str_extract(tif_files, seq(1975, 2030, by = 5) |> paste(collapse = "|"))
  tif_dates <- lubridate::ymd(paste(tif_dates, pgmonth, pgday, sep = "-")) |> as.character()

  names(r) <- tif_dates
  return(r)
}

ghsl_population_grid_uncached <- function(){
  r <- read_ghsl_population_grid()
  res <- robust_transformation(r, agg_fun = "sum")

  #pg <- prio_blank_grid()
  #ragg <- terra::aggregate(r, terra::res(pg)/terra::res(r), fun = "sum")
  #res <- terra::resample(ragg, pg, method = "near")

  return(res)
}

ghsl_population_grid <- memoise::memoise(ghsl_population_grid_uncached, cache = ghsl_cache)

#' Generate GHSL GHS Population Grid
#'
#' This aggregates the high-resolution population grid to PRIO-GRID level across all 5-year intervals (1975 - 2030).
#'
#' This does take some time.
#'
#' A slight nearest neighbor resampling was applied to get the exact PRIO-GRID extent.
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_ghsl_population_grid()
#' @references
#' \insertRef{schiavinaGHSPOPR2023AGHS2023}{priogrid}
gen_ghsl_population_grid <- function(){
  res <- ghsl_population_grid()


  ghsl_dates <- names(res) |> as.Date()
  time_intervals <- pg_date_intervals()

  # If the GHSL data has a lower temporal resolution than PRIO-GRID,
  # we can just return the data.
  pg_interval <- lubridate::int_length(time_intervals[1])
  ghsl_interval <- lubridate::interval(ghsl_dates[2], ghsl_dates[1]) |> lubridate::int_length()
  if(pg_interval <= ghsl_interval){
    return(res)
  }

  # If PRIO-GRID has a lower resolution than GHSL, then
  # we need to aggregate GHSL.
  pg_int <- pg_ints[1]
  relevant_dates <- ghsl_dates[ghsl_dates %within% pg_int]
  r <- mean(res[[as.character(relevant_dates)]])
  for(i in 2:length(pg_ints)){
    pg_int <- pg_ints[i]
    relevant_dates <- ghsl_dates[ghsl_dates %within% pg_int]
    terra::add(r) <- mean(res[[as.character(relevant_dates)]])
  }

  return(res)
}


