# ================================
# Functions for working with 
# rasters
# ================================

library(raster)
library(ncdf4)

get_array <- function(file, variable, fillvarname, lon=NULL, lat=NULL, ...){
  #TODO Needs documentation
  nc <- nc_open(file)

  if(missing(lon) & missing(lat)){
    lon <- NA
    lat <- NA
    res <- NA
  } else{
    lon <- ncvar_get(nc, lon, verbose = F)
    lat <- ncvar_get(nc, lat, verbose = F)
    lonres <- (lon[2]-lon[1])/2
    latres <- (lat[2]-lat[1])/2

  }

  fillvarname <- ncatt_get(nc, varid=variable, attname=fillvarname)

  nc_array <- ncvar_get(nc, variable, ...)

  nc_array[nc_array == fillvarname$value] <- NA
  nc_close(nc)

  return(list("data" = nc_array, 
              "lon" = lon, "lat" = lat, 
              "lonres" = lonres, "latres" = latres))
}

make_raster <- function(nclist, transpose=FALSE, crs=NULL){
  #TODO Needs documentation
  #TODO perhaps a better name? make_raster is too generic

  if(transpose){
     data <- t(nclist$data)
  } else {
     data <- nclist$data
  }

  raster(data, 
      xmn =  min(nclist$lon)-nclist$lonres, 
      xmx = max(nclist$lon)+nclist$lonres,
      ymn = min(nclist$lat)-nclist$latres, 
      ymx = max(nclist$lat)+nclist$latres,
      crs=crs)
}
