### Hyde poPULATION Data

# Load all data, separate files (popc_xx.asc)
pop_hyd_load <- function(path){
  fnames <- paste0("popd_",as.character(c(seq(1950,2000,10),2005)),"AD.asc") 
  files <- file.path(path,fnames)
  rasters <- lapply(files,raster::raster)
  do.call(raster::brick,rasters)
}


#' Generate pop_hyd_sum
#'
#' Sum of number of persons within each grid cell returned as RasterBrick of all available years.

gen_pop_hyd_sum <- function(path){
  hyd <- pop_hyd_load(path)

  pop_hyd_sum <- priogrid::prio_aggregate_raster(hyd, fun = 'sum')
  names(pop_hyd_sum) <- c("pop_hyd_sum_1950", "pop_hyd_sum_1960", "pop_hyd_sum_1970", "pop_hyd_sum_1980", "pop_hyd_sum_1990",
                          "pop_hyd_sum_2000", "pop_hyd_sum_2005")
  return(pop_hyd_sum)
}


#' Generate pop_hyd_sd
#'
#' Standard deviation of original pixel values within each grid cell
#' returned as RasterBrick of all available years.


gen_pop_hyd_sd <- function(path){
  hyd <- pop_hyd_load(path)


  pop_hyd_sd <- priogrid::prio_aggregate_raster(hyd, fun = 'sd')
  names(pop_hyd_sd) <- c("pop_hyd_sd_1950", "pop_hyd_sd_1960", "pop_hyd_sd_1970", "pop_hyd_sd_1980", "pop_hyd_sd_1990",
                         "pop_hyd_sd_2000", "pop_hyd_sd_2005")

  return(pop_hyd_sd)
}



#' Generate pop_hyd_min
#'
#' Minimum of original pixel values within each grid cell
#' returned as RasterBrick of all available years.

gen_pop_hyd_min <- function(path){
  hyd <- pop_hyd_load(path)

  pop_hyd_min <- priogrid::prio_aggregate_raster(hyd, fun = 'min')
  names(pop_hyd_min) <- c("pop_hyd_min_1950", "pop_hyd_min_1960", "pop_hyd_min_1970", "pop_hyd_min_1980", "pop_hyd_min_1990",
                          "pop_hyd_min_2000", "pop_hyd_min_2005")

  return(pop_hyd_min)
}


#' Generate pop_hyd_max
#'
#' Maximum of original pixel values within each grid cell
#' returned as RasterBrick of all available years.

gen_pop_hyd_max <- function(path){
  hyd <- pop_hyd_load(path)

  pop_hyd_max <- priogrid::prio_aggregate_raster(hyd, fun = 'max')

  names(pop_hyd_max) <- c("pop_hyd_max_1950", "pop_hyd_max_1960", "pop_hyd_max_1970", "pop_hyd_max_1980", "pop_hyd_max_1990",
                          "pop_hyd_max_2000", "pop_hyd_max_2005")

  return(pop_hyd_max)
}
