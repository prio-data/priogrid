## TODO Merge on country code?



#' Generate gcp_mer variable
#'
#' Generate gross cell product (USD) variable, based on the G-Econ dataset v. 4.0.
#'
#' @param gecon_data G-Econ v. 4.0 excel data

gen_gcp_mer <- function(gecon_data){

  gcp <- readxl::read_xls(gecon_data, sheet = 1)

  gcp$LAT <- gcp$LAT+0.5
  gcp$LONGITUDE <- gcp$LONGITUDE+0.5

  gcp_1990 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, gcp_1990 = MER1990_40)

  gcp_1995 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, gcp_1995 = MER1995_40)

  gcp_2000 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, gcp_2000 = MER2000_40)

  gcp_2005 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, gcp_2005 = MER2005_40)

  gcp_1990 <- raster::rasterFromXYZ(gcp_1990, crs = priogrid::prio_crs())
  gcp_1995 <- raster::rasterFromXYZ(gcp_1995, crs = priogrid::prio_crs())
  gcp_2000 <- raster::rasterFromXYZ(gcp_2000, crs = priogrid::prio_crs())
  gcp_2005 <- raster::rasterFromXYZ(gcp_2005, crs = priogrid::prio_crs())


  test <- raster::brick(gcp_1990, gcp_1995, gcp_2000, gcp_2005)

  tt <- raster::disaggregate(test, fact = 2)
  return(tt)
}




#' Generate gcp_ppp variable
#'
#' Generate gross cell product (USD adjusted for purchasing-power-parity) variable,
#' based on the G-Econ dataset v. 4.0.
#'
#' @param gecon_data G-Econ v. 4.0 excel data

gen_gcp_ppp <- function(gecon_data){

  gcp <- readxl::read_xls(gecon_data, sheet = 1)

  gcp$LAT <- gcp$LAT+0.5
  gcp$LONGITUDE <- gcp$LONGITUDE+0.5

  gcp_1990 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, ppp_1990 = PPP1990_40)

  gcp_1995 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, ppp_1995 = PPP1995_40)

  gcp_2000 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, ppp_2000 = PPP2000_40)

  gcp_2005 <- gcp %>%
    dplyr::select(LONGITUDE, LAT, ppp_2005 = PPP2005_40)

  gcp_1990 <- raster::rasterFromXYZ(gcp_1990, crs = priogrid::prio_crs())
  gcp_1995 <- raster::rasterFromXYZ(gcp_1995, crs = priogrid::prio_crs())
  gcp_2000 <- raster::rasterFromXYZ(gcp_2000, crs = priogrid::prio_crs())
  gcp_2005 <- raster::rasterFromXYZ(gcp_2005, crs = priogrid::prio_crs())


  test <- raster::brick(gcp_1990, gcp_1995, gcp_2000, gcp_2005)

  tt <- raster::disaggregate(test, fact = 2)
  return(tt)
}



#' Generate gcp_qual variable
#'
#' Generate quality of GCP values variable,
#' based on the G-Econ dataset v. 4.0.
#'
#' @param gecon_data G-Econ v. 4.0 excel data

gen_gcp_qual <- function(gecon_data){

  gcp <- readxl::read_xls(gecon_data, sheet = 1)

  gcp$LAT <- gcp$LAT+0.5
  gcp$LONGITUDE <- gcp$LONGITUDE+0.5

  gcp <- gcp %>%
    dplyr::select(LONGITUDE, LAT, qual = QUALITY)

  gcp <- raster::rasterFromXYZ(gcp, crs = priogrid::prio_crs())

  tt <- raster::disaggregate(gcp, fact = 2)
  return(tt)
}


