gen_gwcode <- function(fname, numCores = 1, quiet = TRUE){
   gwcode <- gen_gwcode_month(fname, numCores, quiet)
   crossection_dates <- names(gwcode)
   crossection_dates <- lubridate::ymd(sub("X", "", crossection_dates))

   crossection_dates <- dplyr::tibble("crossection_date" = crossection_dates, "year" = lubridate::year(crossection_dates))

   first_crossection_in_year <- dplyr::group_by(crossection_dates, year) %>%
      dplyr::arrange(crossection_date) %>%
      dplyr::summarize(crossection_date = first(crossection_date))

   all_years <- dplyr::tibble("year" = 1946:2019)
   all_years <- dplyr::left_join(all_years, first_crossection_in_year, by = "year") %>%
      tidyr::fill(crossection_date)

   i <- 1
   gwcode_year <- list()
   for(year in 1946:2019){
      gwcode_index <- which(crossection_dates$crossection_date == all_years$crossection_date[which(all_years$year == year)])

      gwcode_year[[i]] <- subset(gwcode, gwcode_index)
      i <- i + 1
   }
   gwcode_year <- raster::stack(gwcode_year)
   names(gwcode_year) <- 1946:2019
   return(gwcode_year)
}


#' gen_pgland
#'
#' Takes the weidmann cshapes data set and returns a raster for the
#' grid cells that intersects with land.
#'
#' @param fname File path to Weidmann cshapes data
#' @param quiet Whether or not sf::st_ functions should print warnings.
gen_pgland <- function(fname, quiet = TRUE){
   cshp <- sf::st_read(fname, quiet = quiet)

   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1)

   pg <- priogrid::prio_blank_grid()
   pg_poly <- pg %>% spex::polygonize()

   land_gids <- sf::st_intersects(pg_poly, cshp)
   pgland <- pg_poly[lengths(land_gids)> 0,]
   pgland <- sf::st_centroid(pgland)
   pgland <- raster::rasterize(pgland, pg, field = "layer")
   return(pgland)
}

#' gen_gwcode_month
#'
#' Takes the weidmann cshapes data set and returns a
#' list of rasterlayers for each _changed_ country-
#' border month. Arguments are passed to rasterize_tile and rasterize_worldtiles.
#' Useful arguments to specify
#'
#' @param fname File path to Weidmann cshapes data
#' @param numCores Number of cores to use in calculation. Windows-users can only use 1. Using parallel::mclapply.
#' @param quiet Whether or not sf::st_ functions should print warnings.
gen_gwcode_month <- function(fname, numCores = 1, quiet = quiet){
   calc_crossection <- function(crossection){
      crossection_date <- unique(crossection$crossection_date)
      if(length(crossection_date) != 1){
         print("Non-unique crossection date!")
      }
      print(crossection_date)

      # Global crossection of cshapes used to determine correct gwcode
      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]

      # Gids that have changed since last month
      gids_in_crossection <- sf::st_intersects(pgland, crossection)
      pg_crossection <- pgland[lengths(gids_in_crossection) > 0,]

      # Returns the inner join with the largest area owned in each pg cell that have changed since last month.
      gwcode <- st_join(pg_crossection, cshp_crossection, join = st_intersects, left = FALSE, largest = TRUE) %>%
         dplyr::select(layer, GWCODE)

      return(gwcode)
   }


   compare_crossection <- function(crossection_date, cshp){
      print(crossection_date)
      if(crossection_date - lubridate::month(1) < min(cshp$startdate)){
         changed_areas <- cshp[crossection_date %within% cshp$date_interval,]
         changed_areas$crossection_date <- crossection_date
      } else {
         past_crossection <- cshp[(crossection_date - lubridate::month(1)) %within% cshp$date_interval,]
         cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
         new_changes <- lengths(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0)) == 0

         if(any(new_changes)){
            cshp_crossection$changes <- new_changes
            past_crossection$changes <- lengths(sf::st_equals_exact(past_crossection, cshp_crossection, par = 0)) == 0

            cshp_crossection <- dplyr::filter(cshp_crossection, changes)
            past_crossection <- dplyr::filter(past_crossection, changes)

            changed_areas <- rbind(cshp_crossection, past_crossection)
            changed_areas$crossection_date <- crossection_date

         } else{
            changed_areas <- NULL
         }
      }
      return(changed_areas)
   }



   cshp <- sf::st_read(fname, quiet = quiet)

   # Setting day to first in month to ensure all changes are included.
   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1) %>%
      dplyr::mutate(GWEDAY = 1,
                    GWSDAY = 1) %>%
      dplyr::mutate(
         startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
         enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
      dplyr::mutate(
         date_interval = lubridate::interval(startdate, enddate)
      )

   all_months <- seq(min(cshp$startdate), max(cshp$enddate), by = "1 month")
   # unique_crossections are areas where there have been changes since last month.
   unique_crossections <- parallel::mclapply(all_months, compare_crossection, cshp, mc.cores = numCores)
   unique_crossections[sapply(unique_crossections, is.null)] <- NULL

   pg <- priogrid::prio_blank_grid()
   pgland <- priogrid::gen_pgland(fname, quiet = quiet)

   # ca 22 minutter på 1 kjerne
   # Calculate gwcode-ownership for each cell, for each month where ownership changes somewhere in the world.
   gwcodes <- parallel::mclapply(unique_crossections, calc_crossection, mc.cores = numCores)

   # Update classification scheme iteratively. Gwcodes are only the pg-ids that have changed since last change.
   rasters <- list()
   i <- 1
   current_raster <- pg
   current_raster[] <- NA
   for(j in 1:length(gwcodes)){
      gwcode <- gwcodes[[j]]
      current_raster[match(gwcode$layer, pg[])] <- gwcode$GWCODE
      rasters[[i]] <- current_raster
      i <- i + 1
   }

   gwcode <- raster::stack(rasters)

   crossection_dates <- sapply(unique_crossections, function(x) unique(x$crossection_date))
   crossection_dates <- as.character(as.Date(crossection_dates, origin = as.Date("1970-1-1")))

   names(gwcode) <- crossection_dates
   return(gwcode)
}
