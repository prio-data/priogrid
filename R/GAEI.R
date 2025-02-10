


read_Gaei_files <- function(){
  zip_files <- get_pgfile(source_name = "Global Area Equipped for Irrigation Dataset 1900-2015",
                          source_version = "v4",
                          id = "1604221b-e558-4e65-b7fe-d6b0a517ff5c")

  # unzip(zip_files[1], list = T)

  Gaei_files <- basename(zip_files) |> tools::file_path_sans_ext() |> paste0(".asc")

  for(i in 1:length(zip_files)){
    if(!file.exists(file.path(dirname(zip_files[i]), Gaei_files_files[i]))){
      unzip(zip_files[i], files = Gaei_files_files[i], exdir = dirname(zip_files[i]))
    }
  }

  r <- terra::rast(file.path(dirname(zip_files[i]), Gaei_files_files[1]))
  for(i in 2:length(Gaei_files_files)){
    terra::add(r) <- terra::rast(file.path(dirname(zip_files[i]), Gaei_files_files[i]))
  }

  pgmonth <- pg_dates()[1] |> lubridate::month()
  pgday <- pg_dates()[1] |> lubridate::day()
  tif_dates <- stringr::str_extract(Gaei_files_files, seq(1975, 2030, by = 5) |> paste(collapse = "|"))
  tif_dates <- lubridate::ymd(paste(tif_dates, pgmonth, pgday, sep = "-")) |> as.character()

  names(r) <- tif_dates
  return(r)
}

