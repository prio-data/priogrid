
#' Generate yearly child mortality rate variable (Africa only)
#'
#' Generate variable of average under-5 mortality rate within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param ihme_ IHME mean geoTIFF files for years 2000, 2005, 2010 and 2015

gen_imr_ihme_mean <- function(path){
  agg <- priogrid::prio_aggregate_raster(get_ihme_brick(path), fun = mean)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_mean", years)

  agg
}

gen_imr_ihme_sd <- function(path){
  agg <- priogrid::prio_aggregate_raster(get_ihme_brick(path), fun = sd)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_sd", years)

  agg
}


gen_imr_ihme_max <- function(path){
  agg <- priogrid::prio_aggregate_raster(get_ihme_brick(path), fun = max)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_min", years)

  agg
}

gen_imr_ihme_min <- function(path){
  agg <- priogrid::prio_aggregate_raster(get_ihme_brick(path), fun = min)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_min", years)

  agg
}

prep_imr_imhe <- function(ihme_00, ihme_05, ihme_10, ihme_15){
   i00 <- raster::raster(ihme_00)
   i05 <- raster::raster(ihme_05)
   i10 <- raster::raster(ihme_10)
   i15 <- raster::raster(ihme_15)
   raster::brick(i00,i05,i10,i15)
}
# ================================================

#' get_ihme_brick
#'
#' Returns a raster brick containing the IHME data 
#'
#' @param folder A folder containing the files

get_ihme_brick <- function(folder){
   do.call(prep_imr_imhe,as.list(get_ihme_files(folder)))
}

# ================================================

#' get_ihme_files
#'
#' Returns the paths to the four files needed by the functions
#' generating the ihme raster
#'
#' @param folder A folder containing the files
#' @return A character vector with four paths 

get_ihme_files <- function(folder){
   files <- c("IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2000_Y2017M09D25.TIF",
              "IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2005_Y2017M09D25.TIF",
              "IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2010_Y2017M09D25.TIF",
              "IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2015_Y2017M09D25.TIF")
   file.path(folder,files)
}
