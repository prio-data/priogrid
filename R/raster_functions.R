# ================================
# Functions for working with 
# rasters
# ================================

library(raster)
library(ncdf4)

prio_aggregate_raster <- function(x, fun){
   fact <- resolution_factor(x)
   res <- aggregate(x,fact=fact,fun=fun)
   crs(res) <- prio_crs()
   extent(res) <- prio_extent()
   res
}

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

#' Raster points
#' 
#' Simple function that returns a raster as a sf set of points
raster_points <- function(raster, na.rm = TRUE){

   cids <- 1:length(raster)
   raster_values <- values(raster)
   if(na.rm){
      cids <- cids[!is.na(raster_values)]
      raster_values <- raster_values[!is.na(raster_values)]
   }

   pts <- lapply(cids, function(cid){
      st_point(xyFromCell(raster,cid))
      })

   tibble::tibble(val = raster_values) %>%
     st_sf(geometry = pts) 
}

#' stepwiseAggregate
#'
#' This is a function for aggregating huge rasters.
#' The stepwise aggregation makes it possible to
#' aggregate huge rasters by not having to hold
#' them in memory in their entirety.
#'
#' Use the function "hugeAggregate" instead of this one,
#' since this one will output a raster with an arbitrary
#' instead of a target resolution.
#' 
#' @arg data a rasterlayer 
#' @arg minres won't aggregate past this mininmum res
#' @arg fun what function to use
#' @arg firstpass aggregate first round with min, saving time

stepwiseAggregate <- function(data, minres, fun = priogrid::quickmode, firstpass = TRUE){
   resultant <- length(data) / 4
   if(firstpass){
      tictoc::tic('First pass aggregating with min')
      data <- raster::aggregate(data, fact = 2, fun = min)
      tictoc::toc()
   }

   while(resultant > minres){
      tictoc::tic(glue::glue('Aggregating data of length {length(data)}'))
      data <- raster::aggregate(data, fact = 2, fun = fun)
      tictoc::toc()
      resultant <- length(data) / 4
   }
}
   
