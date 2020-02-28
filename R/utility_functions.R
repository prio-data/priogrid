prio_resolution_factor <- function(x){
  priogrid::prio_resolution()/raster::res(x) # Calculates the factor to aggregate by to get 0.5 x 0.5 resolution
}


prio_aggregate_raster <- function(x, fun){
  fact <- priogrid::prio_resolution_factor(x)
  res <- raster::aggregate(x,fact=fact,fun=fun)
  raster::crs(res) <- priogrid::prio_crs()

  pg <- priogrid::prio_blank_grid()
  raster::values(pg) <- NA

  raster::origin(res) <- raster::origin(pg)

  res <- raster::merge(res, pg, overlap = FALSE)

  res
}
