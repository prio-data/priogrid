
#library(sf)
#library(spex)
#library(tidyverse)
#library(lubridate)
#library(parallel)
#library(raster)

make_gwcode <- function(cshp, ncores = 1){
   pg <- priogrid::prio_blank_grid()
   pg_poly <- priogrid::prio_polygonize_grid(pg)

   cshp <- cshp %>%
     dplyr::filter(GWCODE != -1) %>%
     dplyr::mutate(
       startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
       enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
     dplyr::mutate(
       date_interval = lubridate::interval(startdate, enddate)
     )

   all_months <- seq(min(cshp$startdate), max(cshp$enddate), by = "1 month")
   all_months <- all_months[all_months >= lubridate::ymd("1990-01-01")]
   unique_crossections <- parallel::mclapply(all_months, 
                                   priogrid::compare_crossection, 
                                   cshp=cshp, 
                                   mc.cores = ncores)
   unique_crossections <- dplyr::bind_rows(unique_crossections)

   changedates <- dplyr::filter(unique_crossections, 
                                change == 1 | 
                                unique_crossections$date == unique_crossections$date[1])

   get_gwcode_rasters <- function(crossection_date, cshp){

      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
      cshp_crossection$date_interval <- NULL # Not supported by dplyr, so I remove the column

      cshp_pg <- sf::st_intersection(pg_poly, cshp_crossection)

      cshp_pg$cell_area <- sf::st_area(cshp_pg)
 
      cshp_pg_max <- dplyr::group_by(cshp_pg, pgid) %>%
         dplyr::filter(cell_area == max(cell_area)) %>%
         dplyr::ungroup()

      sf::st_geometry(cshp_pg_max) <- NULL
      cshp_pg_max <- dplyr::select(cshp_pg_max, pgid, GWCODE) %>%
         dplyr::rename(gwcode = GWCODE)
 
      class_df <- tibble::tibble(to = unique(pg[,]), from = unique(pg[,]))
      class_df <- dplyr::left_join(class_df, cshp_pg_max, by = c("to" = "pgid"))
 
      classification_matrix <- as.matrix(class_df)
      rclf <- raster::reclassify(pg, classification_matrix, right = NA)

      return(rclf)
  }

   parallel::mclapply(changedates$date, 
                      get_gwcode_rasters, cshp = cshp, 
                      mc.cores = ncores)
}
