gen_cru <- function(temperaturefile, precipitationfile, petfile, numCores = 1){
  calc_spei <- function(sdf){
    prepet <- sdf$pre - sdf$pet

    if(any(is.na(prepet))){
      sdf$spei1 <- NA
      sdf$spei3 <- NA
      sdf$spei6 <- NA
      sdf$spei12 <- NA
    } else {
      sdf$spei1 <- as.vector(spei(prepet, scale = 1)$fitted)
      sdf$spei3 <- as.vector(spei(prepet, scale = 3)$fitted)
      sdf$spei6 <- as.vector(spei(prepet, scale = 6)$fitted)
      sdf$spei12 <- as.vector(spei(prepet, scale = 12)$fitted)
    }
    return(sdf)
  }

  get_cell_data <- function(longitude, latitude){
    temp_slice <- priogrid::get_array(temperaturefile, variable="tmp", fillvalue="_FillValue", lon = "lon", lat = "lat",
                                      start = c(which(lon == longitude), which(lat == latitude), 1),
                                      count = c(1,1,-1))
    pet_slice <- priogrid::get_array(petfile, variable="pet", fillvalue="_FillValue", lon = "lon", lat = "lat",
                                     start = c(which(lon == longitude), which(lat == latitude), 1),
                                     count = c(1,1,-1))
    pre_slice <- priogrid::get_array(precipitationfile, variable="pre", fillvalue="_FillValue", lon = "lon", lat = "lat",
                                     start = c(which(lon == longitude), which(lat == latitude), 1),
                                     count = c(1,1,-1))

    sdf <- tibble(date = temp_nc_date, lon = longitude, lat = latitude, temp = temp_slice$data, pet = pet_slice$data, pre = pre_slice$data)

    sdf <- calc_spei(sdf)
    return(sdf)
  }

  #### Files ####
  #TEMPERATURE <- "~/Downloads/cru_ts4.03.tmp.dat.nc"
  #PET <- "~/Downloads/cru_ts4.03.pet.dat.nc"
  #PRECIPITATION <- "~/Downloads/cru_ts4.03.pre.dat.nc"

  nc <- nc_open(temperaturefile)
  temp_nc_time <- ncvar_get(nc, "time")
  temp_nc_date <- as.Date(temp_nc_time, origin="1900-1-1")
  lon <- ncvar_get(nc, "lon", verbose = F)
  lat <- ncvar_get(nc, "lat", verbose = F)
  nc_close(nc)

  # Get the data and calculate spei. Returns lists of lists of data frames.
  ldf <- mclapply(lon, function(x) lapply(lat, get_cell_data, longitude = x), mc.cores = numCores)
  df <- bind_rows(do.call(cbind, ldf))

  return(df)
}

