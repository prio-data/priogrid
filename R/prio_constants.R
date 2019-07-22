# PRIO-GRID CONSTANTS

prio_crs <- function(){"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"}
prio_resolution <- function(){0.5}
prio_extent <- function(){raster::extent(-180, 180, -90, 90)}
prio_nrow <- function(){360}
prio_ncol <- function(){720}

prio_midatlantic <- -29


resolution_factor <- function(x){
  priogrid::prio_resolution()/res(x) # Calculates the factor to aggregate by to get 0.5 x 0.5 resolution
}