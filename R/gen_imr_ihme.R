
#' Generate yearly child mortality rate variable (Africa only)
#'
#' Generate variable of average under-5 mortality rate within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param ihme_ IHME mean geoTIFF file for years 2000, 2005, 2010 and 2015
#' @param rast.fun Indicating which variable to be generated (mean, sd, min, max)




gen_imr_ihme <- function(ihme_00, ihme_05, ihme_10, ihme_15, rast.fun){

  i00 <- raster::raster(ihme_00)
  i05 <- raster::raster(ihme_05)
  i10 <- raster::raster(ihme_10)
  i15 <- raster::raster(ihme_15)

  brick <- raster::brick(i00, i05, i10, i15)

  agg <- priogrid::prio_aggregate_raster(brick, fun = rast.fun)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_", rast.fun, "_", years)

  agg
}




