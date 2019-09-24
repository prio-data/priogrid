
#' Generate yearly child mortality rate variable (Africa only)
#'
#' Generate variable of average under-5 mortality rate within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param ihme_ IHME mean geoTIFF files for years 2000, 2005, 2010 and 2015


gen_imr_ihme <- function(ihme_00, ihme_05, ihme_10, ihme_15){
  mean <- gen_imr_ihme_mean(ihme_00, ihme_05, ihme_10, ihme_15)
  sd <- gen_imr_ihme_sd(ihme_00, ihme_05, ihme_10, ihme_15)
  min <- gen_imr_ihme_min(ihme_00, ihme_05, ihme_10, ihme_15)
  max <- gen_imr_ihme_max(ihme_00, ihme_05, ihme_10, ihme_15)

  imr_ihme <- raster::brick(mean, sd, min, max)
  imr_ihme
}


gen_imr_ihme_mean <- function(ihme_00, ihme_05, ihme_10, ihme_15){
  brick <- prep_imr_imhe(ihme_00, ihme_05, ihme_10, ihme_15)

  agg <- priogrid::prio_aggregate_raster(brick, fun = mean)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_mean_", years)

  agg
}

gen_imr_ihme_sd <- function(ihme_00, ihme_05, ihme_10, ihme_15){
  brick <- prep_imr_imhe(ihme_00, ihme_05, ihme_10, ihme_15)

  agg <- priogrid::prio_aggregate_raster(brick, fun = sd)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_sd_", years)

  agg
}


gen_imr_ihme_max <- function(ihme_00, ihme_05, ihme_10, ihme_15){
  brick <- prep_imr_imhe(ihme_00, ihme_05, ihme_10, ihme_15)

  agg <- priogrid::prio_aggregate_raster(brick, fun = max)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_min_", years)

  agg
}

gen_imr_ihme_min <- function(ihme_00, ihme_05, ihme_10, ihme_15){
  brick <- prep_imr_imhe(ihme_00, ihme_05, ihme_10, ihme_15)

  agg <- priogrid::prio_aggregate_raster(brick, fun = min)
  years <- c(2000, 2005, 2010, 2015)
  names(agg) <- paste0("imr_ihme_min_", years)

  agg
}




prep_imr_imhe <- function(ihme_00, ihme_05, ihme_10, ihme_15){
  i00 <- raster::raster(ihme_00)
  i05 <- raster::raster(ihme_05)
  i10 <- raster::raster(ihme_10)
  i15 <- raster::raster(ihme_15)

  brick <- raster::brick(i00, i05, i10, i15)
}
