

#' gen_mountains
#'
#' Uses raw UNEP data to generate a priogrid-sized raster
#' with appropriate(?) heightvalues for each cell.
#'
#' @param data UNEP raw mountains data
gen_mountains <- function(data){
   # made by Peder
   # Functional?

   # The original scale is like this:
   # 1: >= 4500 meters
   # 2: 4500 - 3500 meters
   # 3: 3500 - 2500 meters
   # 4: 2500 - 1500 meters & slipe > 2 deg.
   # 5: 1500 - 1000 meters & (slope >= 5 deg. | local elev. range > 300 m.)
   # 6: 1000 - 300 meters & local elev. range > 300 m.
   # 7: "inner isolated areas" 

   scale <- list()
   dat <- prio_aggregate_raster(data, fun = raster::modal)
   
   # This should be reviewed, OK to assume mean?
   values(dat) <- case_when(
      values(dat) == 1 ~ 4500,
      values(dat) == 2 ~ 4000,
      values(dat) == 3 ~ 3000,
      values(dat) == 4 ~ 2000,
      values(dat) == 5 ~ 1250,
      values(dat) == 6 ~ 650,
      values(dat) == 7 ~ 0)
   dat
}


