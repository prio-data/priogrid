
#library(sf)
#library(spex)
#library(tidyverse)
#library(lubridate)
#library(parallel)
#library(raster)

make_gwcode <- function(input_file, output_file, ncores = 1){
   pg <- priogrid::prio_blank_grid()
   pg_poly <- sf::polygonize(pg)

   cshp <- sf::st_read(input_file)

   cshp <- cshp %>%
     dplyr::filter(GWCODE != -1) %>%
     dplyr::mutate(
       startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
       enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
     dplyr::mutate(
       date_interval = lubridate::interval(startdate, enddate)
     )

   # The stuff below can be done for any monthly cross-section, but we should come
   # up with a smarter algorithm so that we do not need to calculate everything
   # anew every month.
   #### I have just started here with some code that could be a smarter algorithm...
   compare_crossection <- function(crossection_date, cshp){
     if(crossection_date - months(1) < min(cshp$startdate)){
       res <- tibble::tibble(date = crossection_date, change = 1)
     } else {
       past_crossection <- st_combine(cshp[(crossection_date - months(1)) %within% cshp$date_interval,])
       cshp_crossection <- st_combine(cshp[crossection_date %within% cshp$date_interval,])

       if( st_equals_exact(cshp_crossection, past_crossection, par = 0, sparse = F) ){
         res <- tibble::tibble(date = crossection_date, change = 0)
       } else{
         res <- tibble::tibble(date = crossection_date, change = 1)
       }

     }
     return(res)
   }

   all_months <- seq(min(cshp$startdate), max(cshp$enddate), by = "1 month")
   all_months <- all_months[all_months >= ymd("1990-01-01")]
   unique_crossections <- parallel::mclapply(all_months, 
                                   compare_crossection, 
                                   cshp=cshp, 
                                   mc.cores = ncores)

   unique_crossections <- dplyr::bind_rows(unique_crossections)

   sections_with_change_plus_first <- dplyr::filter(unique_crossections, 
                                             change == 1 | unique_crossections$date == unique_crossections$date[1])

   get_gwcode_rasters <- function(crossection_date, cshp){
     cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
     cshp_crossection$date_interval <- NULL # Not supported by dplyr, so I remove the column
     cshp_pg <- sf::st_intersection(pg_poly, cshp_crossection)
     cshp_pg$cell_area <- sf::st_area(cshp_pg)

     cshp_pg_max <- dplyr::group_by(cshp_pg, layer) %>%
       dplyr::filter(cell_area == max(cell_area)) %>%
       dplyr::ungroup()

     sf::st_geometry(cshp_pg_max) <- NULL
     cshp_pg_max <- dplyr::select(cshp_pg_max, layer, GWCODE) %>%
       dplyr::rename(pgid = layer,
              gwcode = GWCODE)

     class_df <- tibble::tibble(to = unique(pg[,]), from = unique(pg[,]))
     class_df <- dplyr::left_join(class_df, cshp_pg_max, by = c("to" = "pgid"))

     classification_matrix <- as.matrix(class_df)
     gwcode <- raster::reclassify(pg, classification_matrix, right = NA)

     # This is only for one cross-section now. Should be yearly rasters, or similar.
     fname <- paste("data_raw/gwcode", crossection_date, ".rda", sep = "")
     save(gwcode, file = fname, compress = TRUE)
     return(crossection_date)
   }

   ldf <- parallel::mclapply(sections_with_change_plus_first$date, 
                   get_gwcode_rasters, cshp = cshp, 
                   mc.cores = ncores)

   saveRDS(ldf,output_file)
}
