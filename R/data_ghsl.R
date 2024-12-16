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

gen_ghsl_population_grid <- function(){
  r <- read_ghsl_population_grid()

  pg <- prio_blank_grid()
  ragg <- terra::aggregate(r, terra::res(pg)/terra::res(r), fun = "sum")
  res <- terra::resample(ragg, pg, method = "near")

  names(res) <- mydate

  return(res)
}
