calc_spei <- function(sdf){
  prepet <- sdf$pre - sdf$pet

  if(any(is.na(prepet))){
    sdf$spei1 <- NA
    sdf$spei3 <- NA
    sdf$spei6 <- NA
    sdf$spei12 <- NA
  } else {
    sdf$spei1 <- as.vector(SPEI::spei(prepet, scale = 1)$fitted)
    sdf$spei3 <- as.vector(SPEI::spei(prepet, scale = 3)$fitted)
    sdf$spei6 <- as.vector(SPEI::spei(prepet, scale = 6)$fitted)
    sdf$spei12 <- as.vector(SPEI::spei(prepet, scale = 12)$fitted)
  }
  return(sdf)
}

gen_cru <- function(input_folder){
  get_cell_data <- function(longitude, latitude){
    temp_slice <- priogrid::get_ncarray(temperature_file, variable="tmp", fillvalue="_FillValue", lon = "lon", lat = "lat",
                                      start = c(which(lon == longitude), which(lat == latitude), 1),
                                      count = c(1,1,-1))
    pet_slice <- priogrid::get_ncarray(pet_file, variable="pet", fillvalue="_FillValue", lon = "lon", lat = "lat",
                                     start = c(which(lon == longitude), which(lat == latitude), 1),
                                     count = c(1,1,-1))
    pre_slice <- priogrid::get_ncarray(precipitation_file, variable="pre", fillvalue="_FillValue", lon = "lon", lat = "lat",
                                     start = c(which(lon == longitude), which(lat == latitude), 1),
                                     count = c(1,1,-1))

    sdf <- tibble::tibble(date = temp_nc_date, lon = longitude, lat = latitude, temp = temp_slice$data, pet = pet_slice$data, pre = pre_slice$data)
    return(sdf)
  }

  #### Files ####
  TEMPERATURE <-  file.path(input_folder, "cru", "data", "cru_ts4.04.1901.2019.tmp.dat.nc.gz")
  PET <-  file.path(input_folder, "cru", "data", "cru_ts4.04.1901.2019.pet.dat.nc.gz")
  PRECIPITATION <-  file.path(input_folder, "cru", "data", "cru_ts4.04.1901.2019.pre.dat.nc.gz")

  temperature_file <- tempfile()
  pet_file <- tempfile()
  precipitation_file <- tempfile()

  R.utils::gunzip(TEMPERATURE, temperature_file, remove = FALSE)
  R.utils::gunzip(PET, pet_file, remove = FALSE)
  R.utils::gunzip(PRECIPITATION, precipitation_file, remove = FALSE)

  nc <- ncdf4::nc_open(temperature_file)
  temp_nc_time <- ncdf4::ncvar_get(nc, "time")
  temp_nc_date <- as.Date(temp_nc_time, origin="1900-1-1")
  lon <- ncdf4::ncvar_get(nc, "lon", verbose = F)
  lat <- ncdf4::ncvar_get(nc, "lat", verbose = F)
  ncdf4::nc_close(nc)

  # Get the data and calculate spei. Returns lists of lists of data frames.
  ldf <- parallel::mclapply(lon, function(x) lapply(lat, get_cell_data, longitude = x))
  df <- lapply(ldf, dplyr::bind_rows)
  df <- dplyr::bind_rows(df)

  unlink(c(temperature_file, pet_file, precipitation_file))
  return(df)
}

