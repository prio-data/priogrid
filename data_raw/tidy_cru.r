library(priogrid)
library(parallel)
library(tidyverse)

# Some preprocessing here is done in CDO at the moment. (Original downloads are netcdf by decade.)
# You can install CDO using conda.
#cdo mergetime cru_ts4.03.*.tmp.dat.nc cru_ts4.03.tmp.dat.nc
#cdo mergetime cru_ts4.03.*.pet.dat.nc cru_ts4.03.pet.dat.nc
#cdo mergetime cru_ts4.03.*.pre.dat.nc cru_ts4.03.pre.dat.nc

TEMPERATURE <- "~/Downloads/cru_ts4.03.tmp.dat.nc"
PET <- "~/Downloads/cru_ts4.03.pet.dat.nc"
PRECIPITATION <- "~/Downloads/cru_ts4.03.pre.dat.nc"

nc <- nc_open(TEMPERATURE)
temp_nc_time <- ncvar_get(nc, "time")
temp_nc_date <- as.Date(temp_nc_time, origin="1900-1-1")
lon <- ncvar_get(nc, "lon", verbose = F)
lat <- ncvar_get(nc, "lat", verbose = F)
nc_close(nc)


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
  temp_slice <- get_array(TEMPERATURE, variable="tmp", fillvalue="_FillValue", lon = "lon", lat = "lat",
                          start = c(which(lon == longitude), which(lat == latitude), 1),
                          count = c(1,1,-1))
  pet_slice <- get_array(PET, variable="pet", fillvalue="_FillValue", lon = "lon", lat = "lat",
                         start = c(which(lon == longitude), which(lat == latitude), 1),
                         count = c(1,1,-1))
  pre_slice <- get_array(PRECIPITATION, variable="pre", fillvalue="_FillValue", lon = "lon", lat = "lat",
                         start = c(which(lon == longitude), which(lat == latitude), 1),
                         count = c(1,1,-1))

  sdf <- tibble(date = temp_nc_date, lon = longitude, lat = latitude, temp = temp_slice$data, pet = pet_slice$data, pre = pre_slice$data)

  sdf <- calc_spei(sdf)
  return(sdf)
}


# Get the data and calculate spei. Returns lists of lists of data frames.
# mc.cores should be adjusted per computer, and this needs quite a lot of RAM (I think more than 32Gb).
ldf <- mclapply(lon, function(x) lapply(lat, get_cell_data, longitude = x), mc.cores = 16)
ldf <- bind_rows(do.call(cbind, ldf))
saveRDS(ldf, "data_raw/cleaned_cru.rds")
