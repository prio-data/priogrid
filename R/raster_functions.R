# ================================
# Functions for working with
# rasters
# ================================

library(raster)
library(ncdf4)

prio_aggregate_raster <- function(x, fun){
   fact <- priogrid::resolution_factor(x)
   res <- raster::aggregate(x,fact=fact,fun=fun)
   raster::crs(res) <- priogrid::prio_crs()

   pg <- priogrid::prio_blank_grid()
   raster::values(pg) <- NA

   raster::origin(res) <- raster::origin(pg)

   res <- raster::merge(res, pg, overlap = FALSE)

   res
}


prio_raster <- function(x){
  # Returns raster with same extent and dimensions as PG, use when aggregation is not needed

  raster::crs(x) <- priogrid::prio_crs()

  pg <- priogrid::prio_blank_grid()
  raster::values(pg) <- NA

  raster::origin(x) <- raster::origin(pg)

  x <- raster::merge(x, pg, overlap = FALSE)

  x

}


get_array <- function(file, variable, fillvalue, lon=NULL, lat=NULL, ...){
  nc <- nc_open(file)

  if(missing(lon) & missing(lat)){
    lon <- NA
    lat <- NA
    res <- NA
  } else{
    lon <- ncvar_get(nc, lon, verbose = F)
    lat <- ncvar_get(nc, lat, verbose = F)
    res <- (lon[2]-lon[1])/2

  } # If I want to add spatial indexing to the function, this information is necessary.

  fillvalue <- ncatt_get(nc, varid=variable, attname=fillvalue)

  nc_array <- ncvar_get(nc, variable, ...)
  nc_array[nc_array == fillvalue$value] <- NA
  nc_close(nc)

  return(list("data" = nc_array, "lon" = lon, "lat" = lat, "res" = res))
}

make_raster <- function(nclist, flip_poles=FALSE, transpose=FALSE, crs=NULL){
  if(transpose){nclist$data <- t(nclist$data)}

  res <- raster(nclist$data, xmn =  min(nclist$lon)-nclist$res, xmx = max(nclist$lon)+nclist$res,
                ymn = min(nclist$lat)-nclist$res, ymx = max(nclist$lat)+nclist$res,
                crs=crs)

  if(flip_poles){res <- flip(res, direction="y")}

  return(res)
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



#' Create RasterBrick consisting of one RasterLayer for each year
#'
#' @param data sf object.
#' @param variable Variable to rasterize, character string.
#' @param raster.fun Function to rasterize by, character string.

yearly_brick <- function(data, variable, raster.fun){
  years <- unique(data$year)
  emp <- list()

  for(i in 1:length(years)) {
    y <- years[i]
    if(sum(data$year == y) > 0)
      emp[[i]] <- raster::rasterize(data[which(data$year == y),],
                                    priogrid::prio_blank_grid(),
                                    field = variable,
                                    fun = raster.fun)
  }
  brick <- raster::brick(emp)
  names(brick) <- paste0(variable,"_", years)
  return(brick)
} ## NOTE: This is fairly quick on smaller point data, but takes a lot of time on larger data.




#' Rotate extent of raster
#'
#' Function to change raster latitudes from (0, 360) to (-180, 180),
#' the former is commonly used in climate data.
#'
#' @param raster RasterLayer or RasterBrick.

rotateExtent <- function(raster){
  ext1 <- raster::extent(0, 180, -90, 90)
  ext2 <- raster::extent(180, 360, -90, 90)

  rightCrop <- raster::crop(raster, ext1)

  leftCrop <- raster::crop(raster, ext2)

  full <- raster::merge(rightCrop, leftCrop)
  full <- raster::rotate(full)

  full
}

