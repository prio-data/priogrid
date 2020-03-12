monthly_cshp <- function(input_folder){
   cshp <- sf::read_sf(file.path(input_folder, "cshapes", "data", "cshapes.shp"))
   names(cshp) <- tolower(names(cshp))

   cshp <- cshp %>%
      dplyr::filter(gwcode != -1) %>%
      dplyr::mutate(
         startdate = lubridate::ymd(paste(gwsyear, gwsmonth, gwsday, sep = "-")),
         enddate = lubridate::ymd(paste(gweyear, gwemonth, gweday, sep = "-"))) %>%
      dplyr::mutate(
         date_interval = lubridate::interval(startdate, enddate)
      )
   return(cshp)
}




#' gen_gwcode
#'
#' Takes the weidmann cshapes data set and returns a
#' rasterstack for each year that codes the country ownership of each cell
#' as it looked like 1 January each year.
#'
#' @param fname File path to Weidmann cshapes data
#' @param numCores Number of cores to use in calculation. Windows-users can only use 1. Using parallel::mclapply.
#' @param quiet Whether or not sf::st_ functions should print warnings.
gen_gwcode <- function(fname, numCores = 1, quiet = TRUE){
   # Is this the correct way to select?
   gwcode <- gen_gwcode_changes(fname, numCores, quiet)
   crossection_dates <- names(gwcode)
   crossection_dates <- lubridate::ymd(sub("X", "", crossection_dates))

   all_dates <- seq(lubridate::ymd("1946-1-1"), lubridate::ymd("2019-1-1"), by = "1 year")
   all <- dplyr::tibble("mydate" = all_dates, "crossection_date" = NA)

   for(current_date in all$mydate){
      closest_matching_date <- max(crossection_dates[crossection_dates <= current_date])
      all$crossection_date[which(all$mydate == current_date)] <- closest_matching_date
   }
   all$year <- lubridate::year(all$mydate)
   all$crossection_date <- as.Date(all$crossection_date, origin = as.Date("1970-1-1"))

   i <- 1
   gwcode_year <- list()
   for(year in 1946:2019){
      gwcode_index <- which(crossection_dates == all$crossection_date[which(all$year == year)])

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
gen_pgland <- function(input_folder, quiet = TRUE){
   cshp <- sf::read_sf(file.path(input_folder, "cshapes", "data", "cshapes.shp"))

   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1)

   pg <- priogrid::prio_blank_grid()
   pg_poly <- pg %>% spex::polygonize()

   land_gids <- sf::st_intersects(pg_poly, cshp)
   pgland <- pg_poly[lengths(land_gids)> 0,]
   pgland <- dplyr::tibble("pgid" = pgland$pgid, "pgland" = 1L)
   return(pgland)
}

gen_landarea_sf <- function(input_folder){
   pgland <- file.path(input_folder, "cshapes", "cache", "pgland.parquet")
   assertthat::assert_that(file.exists(pgland))
   pgland <- arrow::read_parquet(pgland)

   pg <- priogrid::prio_blank_grid()
   pg[!(values(pg) %in% pgland$pgid)] <- NA

   pgland <- spex::polygonize(pg)

   cshp <- sf::read_sf(file.path(input_folder, "cshapes", "data", "cshapes.shp"))

   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1, GWEYEAR == max(GWEYEAR))

   land_polygons <- sf::st_intersection(pgland, cshp)
   land_polygons$pgarea <- sf::st_area(land_polygons)
   st_crs(land_polygons) <- st_crs(pg)
   return(land_polygons)
}

gen_landarea <- function(input_folder){
   pgarea <- gen_landarea_sf(input_folder)
   sf::st_geometry(pgarea) <- NULL

   pgarea <- dplyr::group_by(pgarea, pgid) %>%
     dplyr::summarise(pgarea = sum(pgarea, na.rm = T))
   return(pgarea)
}

#' gen_changed_areas
#'
#' Takes the weidmann cshapes data set and returns a
#' list of sf dataframes that corresponds to the areas where borders changed from one month to the next.
#'
#' @param fname File path to Weidmann cshapes data
#' @param numCores Number of cores to use in calculation. Windows-users can only use 1. Using parallel::mclapply.
#' @param quiet Whether or not sf::st_ functions should print warnings.
gen_changed_areas <- function(input_folder){
   compare_crossection <- function(crossection_date, cshp, dates_with_changes){
      message(crossection_date)
      if(which(crossection_date == dates_with_changes) == 1){
         changed_areas <- cshp[crossection_date %within% cshp$date_interval,]
         changed_areas$crossection_date <- crossection_date
      } else {
         last_date <- dates_with_changes[which(crossection_date == dates_with_changes) - 1]
         past_crossection <- cshp[last_date %within% cshp$date_interval,]
         cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
         new_changes <- lengths(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0)) == 0

         if(any(new_changes)){
            cshp_crossection$changes <- new_changes
            cshp_crossection <- dplyr::filter(cshp_crossection, changes)
            # Combine and buffer to make sure area-calculations are done again for bordering cells. st_union to check validity.
            geometry <- sf::st_union(sf::st_buffer(sf::st_combine(cshp_crossection), 1))
            changed_areas <- sf::st_sf(geometry)
            changed_areas$crossection_date <- crossection_date

         } else{
            changed_areas <- NULL
         }
      }
      return(changed_areas)
   }

   cshp <- priogrid::monthly_cshp(input_folder)


   dates_with_changes <- sort(unique(unique(cshp$startdate), unique(cshp$enddate)))
   #Find all areas and dates where there have been changes since last day.
   changed_areas <- parallel::mclapply(dates_with_changes, compare_crossection, cshp, dates_with_changes)
   changed_areas[sapply(changed_areas, is.null)] <- NULL

   crossection_dates <- sapply(changed_areas, function(x) unique(x$crossection_date))
   crossection_dates <- as.character(as.Date(crossection_dates, origin = as.Date("1970-1-1")))

   names(changed_areas) <- crossection_dates


   return(changed_areas)
}

#' gen_gwcode_changes
#'
#' Takes the weidmann cshapes data set and returns a
#' rasterstack that encodes the country ownership of a cell as it looked like on the 1st each month.
#' The rasterstack only includes the months where there was a change from one month to the next.
#'
#' @param fname File path to Weidmann cshapes data
#' @importFrom lubridate %within%
gen_gwcode_changes <- function(input_folder){
   calc_crossection <- function(crossection, dates_with_changes){
      crossection_date <- unique(crossection$crossection_date)
      assertthat::assert_that(length(crossection_date) == 1)

      if(which(crossection_date == dates_with_changes) != 1){
         past_crossection <- cshp[(crossection_date - lubridate::month(1)) %within% cshp$date_interval,]
      } else {
         last_date <- dates_with_changes[which(crossection_date == dates_with_changes) - 1]
         past_crossection <- cshp[last_date %within% cshp$date_interval,]
      }

      # Global crossection of cshapes used to determine correct gwcode
      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
      new_changes <- lengths(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0)) == 0
      cshp_crossection$changes <- new_changes
      cshp_crossection <- dplyr::filter(cshp_crossection, changes)

      # Gids that have changed since last month
      gids_in_crossection <- sf::st_intersects(pgland, crossection)
      pg_crossection <- pgland[lengths(gids_in_crossection) > 0,]

      # Returns the inner join with the largest area owned in each pg cell that have changed since last month.
      gwcodes <- sf::st_join(pg_crossection, cshp_crossection, join = sf::st_intersects, left = FALSE, largest = TRUE) %>%
         dplyr::select(pgid, gwcode)

      return(gwcodes)
   }

   pgland <- file.path(input_folder, "cshapes", "cache", "pgland.parquet")
   assertthat::assert_that(file.exists(pgland))
   pgland <- arrow::read_parquet(pgland)
   pg <- priogrid::prio_blank_grid()
   pg[!(raster::values(pg) %in% pgland$pgid)] <- NA
   pgland <- spex::polygonize(pg)

   changed_areas <- priogrid::gen_changed_areas(input_folder)
   cshp <- priogrid::monthly_cshp(input_folder)
   dates_with_changes <- lubridate::ymd((names(changed_areas)))

   # Calculate gwcode-ownership for each cell, for each month where ownership changes somewhere in the world.
   gwcodes <- parallel::mclapply(changed_areas, calc_crossection, dates_with_changes)
   gwcodes <- priogrid::update_cells_iteratively(gwcodes, "gwcode", changed_areas)
   return(gwcodes)
}

gen_coastdist <- function(input_folder){
   pgland <- file.path(input_folder, "cshapes", "cache", "pgland.parquet")
   assertthat::assert_that(file.exists(pgland))
   pgland <- arrow::read_parquet(pgland)

   pgland <- priogrid::raster_to_tibble(priogrid::prio_blank_grid()) %>% dplyr::filter(pgid %in% pgland$pgid)
   pgland <- sf::st_as_sf(pgland, coords = c("x", "y"))
   sf::st_crs(pgland) <- priogrid::prio_crs()

   # We use GSHHG, with the "low" resolution. Higher resolution data is available if necessary. (GSHHS_c/f/h/i/l_L1.shp)
   coastline <- file.path(input_folder, "gshhg", "data", "GSHHS_shp", "l", "GSHHS_l_L1.shp")
   coastline <- sf::read_sf(coastline)
   coastline <- sf::st_boundary(coastline)

   pgland$coastdist <- priogrid::get_closest_distance(pgland, coastline)

   sf::st_geometry(pgland) <- NULL
   return(pgland)
}

gen_riverdist <- function(input_folder){
   pgland <- file.path(input_folder, "cshapes", "cache", "pgland.parquet")
   assertthat::assert_that(file.exists(pgland))
   pgland <- arrow::read_parquet(pgland)

   pgland <- priogrid::raster_to_tibble(priogrid::prio_blank_grid()) %>% dplyr::filter(pgid %in% pgland$pgid)
   pgland <- sf::st_as_sf(pgland, coords = c("x", "y"))
   sf::st_crs(pgland) <- priogrid::prio_crs()

   river1 <- file.path(input_folder, "gshhg", "data", "WDBII_shp", "l", "WDBII_river_l_L01.shp")
   river2 <- file.path(input_folder, "gshhg", "data", "WDBII_shp", "l", "WDBII_river_l_L02.shp")
   river3 <- file.path(input_folder, "gshhg", "data", "WDBII_shp", "l", "WDBII_river_l_L03.shp")
   lakes <- file.path(input_folder, "gshhg", "data", "GSHHS_shp", "l", "GSHHS_l_L2.shp")

   river1 <- sf::read_sf(river1)
   river2 <- sf::read_sf(river2)
   river3 <- sf::read_sf(river3)
   lakes <- sf::read_sf(lakes)
   lakes <- dplyr::filter(lakes, area >= 1000)

   lakes_and_rivers <- rbind(dplyr::select(river1, geometry),
                             dplyr::select(river2, geometry),
                             dplyr::select(river3, geometry),
                             dplyr::select(lakes, geometry))


   pgland$riverdist <- priogrid::get_closest_distance(pgland, lakes_and_rivers)

   sf::st_geometry(pgland) <- NULL
   return(pgland)
}


gen_bdist1_changes <- function(input_folder){
   #bdist1: distance in km from the centroid to the border of the nearest land-contiguous neighboring country.
   bdist1_on_crossection <- function(crossection){
      crossection_date <- unique(crossection$crossection_date)
      message(crossection_date)

      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]
      land_intersections <- sf::st_intersects(cshp_cross)

      # Drop self-reference
      for(i in 1:length(land_intersections)){
         land_intersections[i][[1]] <- land_intersections[i][[1]][land_intersections[i][[1]] != i]
      }

      # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
      gw_crossection <- dplyr::filter(gwcode_changes, mydate == crossection_date)
      gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("x", "y"))
      sf::st_crs(gw_crossection) <- priogrid::prio_crs()

      land_codes <- unique(gw_crossection$gwcode)
      get_distances <- function(land_code){
         land_intersection_index <- which(cshp_cross$gwcode == land_code)
         neighbor_index <- land_intersections[land_intersection_index][[1]]
         neighbors <- cshp_cross[neighbor_index, ]
         return(priogrid::get_closest_distance(dplyr::filter(gw_crossection, gwcode == land_code), neighbors))
      }
      distances_cross <- parallel::mclapply(land_codes, get_distances)

      gw_crossection$bdist1 <- NA
      for(i in 1:length(land_codes)){
         gw_crossection[gw_crossection$gwcode==land_codes[i],"bdist1"] <- distances_cross[[i]]
      }

      sf::st_geometry(gw_crossection) <- NULL
      gw_crossection$mydate <- NULL

      return(gw_crossection)
   }

   gwcode_changes <- file.path(input_folder, "cshapes", "cache", "gwcode_changes.parquet")
   assertthat::assert_that(file.exists(gwcode_changes))
   gwcode_changes <- arrow::read_parquet(gwcode_changes)
   gwcode_changes$mydate <- lubridate::ymd(paste(gwcode_changes$year, gwcode_changes$month, gwcode_changes$day, sep = "-"))
   pg <- priogrid::raster_to_tibble(priogrid::prio_blank_grid())
   gwcode_changes <- dplyr::left_join(gwcode_changes, pg, by = c("x", "y")) # add priogrid-id

   cshp <- priogrid::monthly_cshp(input_folder)
   changed_areas <- priogrid::gen_changed_areas(input_folder)

   bdist1 <- lapply(changed_areas, bdist1_on_crossection) # calculate distances
   bdist1 <- priogrid::update_cells_iteratively(bdist1, "bdist1", changed_areas)
   return(bdist1)
}


gen_bdist2_month <- function(input_folder){
   #bdist2: distance in km from the centroid to the border of the nearest neighboring country.
   bdist2_on_crossection <- function(crossection){
      crossection_date <- unique(crossection$crossection_date)
      message(crossection_date)

      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

      # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
      gw_crossection <- dplyr::filter(gwcode_changes, mydate == crossection_date)
      gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("x", "y"))
      sf::st_crs(gw_crossection) <- priogrid::prio_crs()

      land_codes <- unique(gw_crossection$gwcode)
      get_distances <- function(land_code){
         neighbors <- cshp_cross[which(cshp_cross$gwcode != land_code), ]
         return(priogrid::get_closest_distance(dplyr::filter(gw_crossection, gwcode == land_code), neighbors))
      }
      distances_cross <- parallel::mclapply(land_codes, get_distances)

      gw_crossection$bdist2 <- NA
      for(i in 1:length(land_codes)){
         gw_crossection[gw_crossection$gwcode==land_codes[i],"bdist2"] <- distances_cross[[i]]
      }

      sf::st_geometry(gw_crossection) <- NULL
      gw_crossection$mydate <- NULL

      return(gw_crossection)
   }

   gwcode_changes <- file.path(input_folder, "cshapes", "cache", "gwcode_changes.parquet")
   assertthat::assert_that(file.exists(gwcode_changes))
   gwcode_changes <- arrow::read_parquet(gwcode_changes)
   gwcode_changes$mydate <- lubridate::ymd(paste(gwcode_changes$year, gwcode_changes$month, gwcode_changes$day, sep = "-"))
   pg <- priogrid::raster_to_tibble(priogrid::prio_blank_grid())
   gwcode_changes <- dplyr::left_join(gwcode_changes, pg, by = c("x", "y")) # add priogrid-id

   cshp <- priogrid::monthly_cshp(input_folder)
   changed_areas <- priogrid::gen_changed_areas(input_folder)

   bdist2 <- lapply(changed_areas, bdist2_on_crossection) # calculate distances
   bdist2 <- priogrid::update_cells_iteratively(bdist2, "bdist2", changed_areas)
   return(bdist2)
}


gen_bdist3_month <- function(input_folder){
   #bdist3: distance in km from the centroid to the territorial outline of the country the cell belongs to.
   bdist3_on_crossection <- function(crossection){
      crossection_date <- unique(crossection$crossection_date)
      message(crossection_date)

      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

      # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
      gw_crossection <- dplyr::filter(gwcode_changes, mydate == crossection_date)
      gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("x", "y"))
      sf::st_crs(gw_crossection) <- priogrid::prio_crs()

      gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
      gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]

      land_codes <- unique(gw_crossection$gwcode)

      for(i in 1:length(land_codes)){
         this_country <- cshp_cross[which(cshp_cross$gwcode == land_codes[i]), ]
         centroid_within_country <- sf::st_within(dplyr::filter(gw_crossection, gwcode == land_codes[i]), this_country)
         this_country <- sf::st_boundary(this_country)
         gw_crossection$cwc <- NA
         gw_crossection$cwc[gw_crossection$gwcode == land_codes[i]] <- lengths(centroid_within_country) > 0

         # Cells belonging to a country, but with a centroid outside the border will have 0 distance to the outline of the country.
         gw_crossection[which(gw_crossection$gwcode==land_codes[i] & gw_crossection$cwc == FALSE), "bdist3"] <- 0

         gw_crossection[which(gw_crossection$gwcode==land_codes[i] & gw_crossection$cwc == TRUE),"bdist3"] <- priogrid::get_closest_distance(
            gw_crossection[which(gw_crossection$gwcode==land_codes[i] & gw_crossection$cwc == TRUE),], this_country)

      }

      sf::st_geometry(gw_crossection) <- NULL
      gw_crossection$mydate <- NULL
      gw_crossection$cwc <- NULL

      return(gw_crossection)
   }

   gwcode_changes <- file.path(input_folder, "cshapes", "cache", "gwcode_changes.parquet")
   assertthat::assert_that(file.exists(gwcode_changes))
   gwcode_changes <- arrow::read_parquet(gwcode_changes)
   gwcode_changes$mydate <- lubridate::ymd(paste(gwcode_changes$year, gwcode_changes$month, gwcode_changes$day, sep = "-"))
   pg <- priogrid::raster_to_tibble(priogrid::prio_blank_grid())
   gwcode_changes <- dplyr::left_join(gwcode_changes, pg, by = c("x", "y")) # add priogrid-id

   cshp <- priogrid::monthly_cshp(input_folder)
   changed_areas <- priogrid::gen_changed_areas(input_folder)

   bdist3 <- parallel::mclapply(changed_areas, bdist3_on_crossection) # calculate distances
   bdist3 <- priogrid::update_cells_iteratively(bdist3, "bdist3", changed_areas)
   return(bdist3)
}


#message("capdist: distance in km from the cell centroid to the national capital in the country the cell belongs to")
gen_capdist <- function(fname, output_folder, numCores = 1, quiet = TRUE){
   calc_crossection <- function(crossection, all_months, gwcode, cshp){
      crossection_date <- unique(crossection$crossection_date)
      message(crossection_date)

      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]

      gwcode_index <- which(all_months %in% crossection_date)
      gw_crossection <- raster::subset(gwcode, gwcode_index)

      gw_crossection <- raster::rasterToPoints(gw_crossection)
      gw_crossection <- dplyr::tibble("gwcode" = gw_crossection[,3], "lon" = gw_crossection[,1], "lat" = gw_crossection[,2])
      gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("lon", "lat"))
      sf::st_crs(gw_crossection) <- sf::st_crs(4326)

      # Gids that have changed since last month
      gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
      gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]


      gw_crossection$capdist <- NA
      for(country_id in unique(gw_crossection$gwcode)){
         gw_crossection$capdist[which(gw_crossection$gwcode == country_id)] <- priogrid::get_closest_distance(
            gw_crossection[which(gw_crossection$gwcode == country_id),],
            cshp_crossection[which(cshp_crossection$GWCODE == country_id),])
      }


      pg <- priogrid::prio_blank_grid()
      capdist <- raster::rasterize(gw_crossection, pg, field = "capdist")
      return(capdist)
   }

   cshp <- sf::st_read(fname, quiet = T)
   cshp_crs <- sf::st_crs(cshp)
   sf::st_geometry(cshp) <- NULL
   cshp <- sf::st_as_sf(cshp, coords = c("CAPLONG", "CAPLAT"))
   sf::st_crs(cshp) <- cshp_crs

   # Setting day to first in month to ensure all changes are included.
   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1) %>%
      dplyr::mutate(
         startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
         enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
      dplyr::mutate(
         same_month = year(startdate) == year(enddate) & month(startdate) == month(enddate)
      ) %>%
      dplyr::filter(same_month == F & startdate != "1991-12-16") %>% # Hack for now. Multiple changes within each month.
      dplyr::mutate(GWSDAY = 1,
                    GWEDAY = 1) %>%
      dplyr::mutate(
         startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
         enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
 #     dplyr::mutate(
 #        enddate = enddate - lubridate::days(1) # End up until, but not including
 #     ) %>%
      dplyr::mutate(
         date_interval = lubridate::interval(startdate, enddate)
      )

   message("Get the set of grid cells that intersect with land from file.")
   gwcode_file <- paste0(output_folder, "gwcode_changes.rds")
   if(!is.null(output_folder) &  file.exists(gwcode_file)){
      gwcode <- readRDS(gwcode_file)
   } else{
      return(paste(gwcode_file, "does not exist. Please calculate gwcode_changes first."))
   }

   message("Get the countries where the border changed from one month to the next.")
   changed_areas_file <- paste0(output_folder, "changed_areas.rds")
   if(!is.null(output_folder) &  file.exists(changed_areas_file)){
      changed_areas <- readRDS(changed_areas_file)
   } else{
      break(paste(changed_areas_file, "does not exist. Please calculate changed_areas first."))
   }

   all_months <- lubridate::ymd(sub("X", "", names(gwcode)))

   capdist <- parallel::mclapply(changed_areas, calc_crossection, all_months = all_months, gwcode = gwcode, cshp = cshp, mc.cores = numCores)

   pg <- priogrid::prio_blank_grid()
   # Update classification scheme iteratively. Gwcodes are only the pg-ids that have changed since last change.
   rasters <- list()
   i <- 1
   current_raster <- pg
   current_raster[] <- NA
   for(j in 1:length(capdist)){
      dc <- capdist[[j]]

      current_raster[!is.na(dc)] <- dc[!is.na(dc)]
      rasters[[i]] <- current_raster
      i <- i + 1
   }

   capdist <- raster::stack(rasters)

   crossection_dates <- sapply(changed_areas, function(x) unique(x$crossection_date))
   crossection_dates <- as.character(as.Date(crossection_dates, origin = as.Date("1970-1-1")))

   names(capdist) <- crossection_dates
   return(capdist)
}
