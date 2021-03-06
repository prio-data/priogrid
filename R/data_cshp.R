#' monthly_cshp
#'
#' cleans cshp data
#'
#' @param input_folder
#'
#' @return a sf dataframe
#' @export
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





#' pg_timeseries_from_changes
#'
#'#' Takes the weidmann cshapes data set and returns a
#' rasterstack for each year that codes the country ownership of each cell
#' as it looked like 1 January each year.
#'
#' @param input_folder
#' @param interval see by argument in base::seq.Date
pg_timeseries_from_changes <- function(changes_df, interval, enddate, startdate){

   changes_df <- dplyr::mutate(changes_df,
                                   mydate = lubridate::ymd(paste(year, month, day, sep = "-")))

   if(!is.null(startdate)){
      changes_df <- dplyr::filter(changes_df, mydate >= startdate)
   }

   first_date <- min(changes_df$mydate)
   last_date <- max(changes_df$mydate)

   if(!is.null(enddate)){
      last_date <- enddate
   }

   all_years <- seq(first_date, last_date, by = interval)

   filter_to_year <- function(current_date){
      closest_matching_date <- dplyr::filter(changes_df, mydate <= current_date) %>%
         dplyr::summarize(closest_date = max(mydate)) %>% dplyr::pull(closest_date)

      res <- dplyr::filter(changes_df, mydate == closest_matching_date)
      res$mydate <- current_date
      res$year <- NULL
      res$month <- NULL
      res$day <- NULL
      return(res)
   }

   pg_timeseries <- parallel::mclapply(all_years, filter_to_year) %>% dplyr::bind_rows()
   return(pg_timeseries)
}


#' gen_gwcode
#'
#' Takes gwcode_changes and translates it into a regular time-series.
#'
#' @param input_folder
#' @param interval see by argument in base::seq.Date. default is 1 year.
#' @param enddate a date. option to add more years than the last entry in the input data
#' @param startdate a date. filters the input data to start at the given date.
#' @export
gen_gwcode <- function(input_folder, input_file = "gwcode_changes.parquet", interval = "1 year", enddate = as.Date("2020-1-1"), startdate = NULL){
   changes_df <- file.path(input_folder, "cshapes", "cache", input_file)
   assertthat::assert_that(file.exists(changes_df))
   changes_df <- arrow::read_parquet(changes_df)
   regular_timeseries <- pg_timeseries_from_changes(changes_df, interval = interval, enddate = enddate, startdate = startdate)
   return(regular_timeseries)
}

#' gen_bdist1
#'
#' Takes bdist1_changes and translates it into a regular time-series.
#'
#' @param input_folder
#' @param interval see by argument in base::seq.Date. default is 1 year.
#' @param enddate a date. option to add more years than the last entry in the input data
#' @param startdate a date. filters the input data to start at the given date.
#' @export
gen_bdist1 <- function(input_folder, ...){
   gen_gwcode(input_folder, input_file = "bdist1_changes.parquet", ...)
}

#' gen_bdist2
#'
#' Takes bdist2_changes and translates it into a regular time-series.
#'
#' @param input_folder
#' @param interval see by argument in base::seq.Date. default is 1 year.
#' @param enddate a date. option to add more years than the last entry in the input data
#' @param startdate a date. filters the input data to start at the given date.
#' @export
gen_bdist2 <- function(input_folder, ...){
   gen_gwcode(input_folder, input_file = "bdist2_changes.parquet", ...)
}

#' gen_bdist3
#'
#' Takes bdist3_changes and translates it into a regular time-series.
#'
#' @param input_folder
#' @param interval see by argument in base::seq.Date. default is 1 year.
#' @param enddate a date. option to add more years than the last entry in the input data
#' @param startdate a date. filters the input data to start at the given date.
#' @export
gen_bdist3 <- function(input_folder, ...){
   gen_gwcode(input_folder, input_file = "bdist3_changes.parquet", ...)
}

#' gen_capdist
#'
#' Takes capdist_changes and translates it into a regular time-series.
#'
#' @param input_folder
#' @param interval see by argument in base::seq.Date. default is 1 year.
#' @param enddate a date. option to add more years than the last entry in the input data
#' @param startdate a date. filters the input data to start at the given date.
#' @export
gen_capdist <- function(input_folder, ...){
   gen_gwcode(input_folder, input_file = "capdist_changes.parquet", ...)
}

#' gen_pgland
#'
#' Takes the weidmann cshapes data set and returns a raster for the
#' grid cells that intersects with land.
#'
#' @param input_folder path to PRIO-GRID input data
#' @export
gen_pgland <- function(input_folder){
   cshp <- sf::read_sf(file.path(input_folder, "cshapes", "data", "cshapes.shp"))

   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1, GWEYEAR == max(GWEYEAR))

   pg <- priogrid::prio_blank_grid()
   pg_poly <- pg %>% spex::polygonize()

   sf::st_crs(cshp) <- sf::st_crs(pg_poly)
   land_gids <- sf::st_intersects(pg_poly, cshp)
   pgland <- pg_poly[lengths(land_gids)> 0,]
   pgland <- dplyr::tibble("pgid" = pgland$pgid, "pgland" = 1L)
   return(pgland)
}

#' @export
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

#' gen_landarea
#'
#'
#' @input_folder input_folder Folder path to PRIO-GRID input data
#' @export
gen_landarea <- function(input_folder){
   pgarea <- gen_landarea_sf(input_folder)
   sf::st_geometry(pgarea) <- NULL

   pgarea <- dplyr::group_by(pgarea, pgid) %>%
     dplyr::summarise(pgarea = sum(pgarea, na.rm = T))
   return(pgarea)
}

#' gen_changed_areas
#'
#' Finds the areas and dates where borders have changed in the cshapes dataset.
#'
#' @param input_folder
#'
#' @return a list, one sf df for each crossection
#' @export
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
#' @param input_folder Path to PRIO-GRID input data
#' @export
gen_gwcode_changes <- function(input_folder){
   calc_crossection <- function(crossection, dates_with_changes){
      crossection_date <- unique(crossection$crossection_date)
      assertthat::assert_that(length(crossection_date) == 1)

      if(which(crossection_date == dates_with_changes) == 1){
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

#' gen_coastdist
#'
#' Calculates the distance between cell centroids and the coast.
#'
#' @param input_folder path to PRIO-GRID input data
#'
#' @return a tibble, with x, y, pgid, and coastdist
#' @export
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

#' gen_riverdist
#'
#' Calculates the distance between cell centroids and the nearest major river or lake.
#'
#' @param input_folder path to PRIO-GRID input data
#'
#' @return a tibble, with x, y, pgid, and riverdist
#' @export
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


#' @export
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

      gw_crossection$bdist1 <- NA_real_
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


#' @export
gen_bdist2_changes<- function(input_folder){
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

      gw_crossection$bdist2 <- NA_real_
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


#' @export
gen_bdist3_changes <- function(input_folder){
   #bdist3: distance in km from the centroid to the territorial outline of the country the cell belongs to.
   bdist3_on_crossection <- function(crossection){
      crossection_date <- unique(crossection$crossection_date)
      message(crossection_date)

      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

      # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
      gw_crossection <- dplyr::filter(gwcode_changes, mydate == crossection_date)
      gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
      gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]

      # Set geometry to cell centroid
      sf::st_geometry(gw_crossection) <- NULL
      gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("x", "y"))
      sf::st_crs(gw_crossection) <- priogrid::prio_crs()


      land_codes <- unique(gw_crossection$gwcode)


      get_distances <- function(land_code){
         this_country <- cshp_cross[which(cshp_cross$gwcode == land_code), ]
         centroid_within_country <- sf::st_within(dplyr::filter(gw_crossection, gwcode == land_code), this_country)
         this_country <- sf::st_boundary(this_country)

         distances <- priogrid::get_closest_distance(gw_crossection[which(gw_crossection$gwcode==land_code),], this_country)

         # Cells belonging to a country, but with a centroid outside the border will have 0 distance to the outline of the country.
         distances[!(lengths(centroid_within_country) > 0)] <- 0
         return(distances)
      }

      distances_cross <- parallel::mclapply(land_codes, get_distances)

      gw_crossection$bdist3 <- NA_real_
      for(i in 1:length(land_codes)){
         gw_crossection[gw_crossection$gwcode==land_codes[i],"bdist3"] <- distances_cross[[i]]
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
   pg <- priogrid::prio_blank_grid()
   pg_poly <- pg %>% spex::polygonize()
   gwcode_changes <- dplyr::left_join(gwcode_changes, pg_poly, by = "pgid") %>% sf::st_as_sf()

   cshp <- priogrid::monthly_cshp(input_folder)
   changed_areas <- priogrid::gen_changed_areas(input_folder)

   bdist3 <- lapply(changed_areas, bdist3_on_crossection) # calculate distances
   bdist3 <- priogrid::update_cells_iteratively(bdist3, "bdist3", changed_areas)
   return(bdist3)
}


#message("capdist: distance in km from the cell centroid to the national capital in the country the cell belongs to")

#' @export
gen_capdist_changes <- function(input_folder){
   calc_crossection <- function(crossection, dates_with_changes){
      crossection_date <- unique(crossection$crossection_date)
      assertthat::assert_that(length(crossection_date) == 1)


      # Point data for capitals as they looked like during the cross-section
      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]

      new_or_different_capital_location <- c()
      # Check if capitals have moved
      if(which(crossection_date == dates_with_changes) != 1){
         last_date <- dates_with_changes[which(crossection_date == dates_with_changes) - 1]
         past_crossection <- cshp[last_date %within% cshp$date_interval,]

         past <- past_crossection %>% dplyr::select(gwcode)
         past_xy <- sf::st_coordinates(past)
         past <- sf::st_drop_geometry(past) %>% cbind(past_xy)
         current <- cshp_crossection %>% dplyr::select(gwcode)
         current_xy <- sf::st_coordinates(current)
         current <- sf::st_drop_geometry(current) %>% cbind(current_xy)
         new_or_different_capital_location <- dplyr::anti_join(current, past)
      }

      # Cell-centroids for gids that have changed since last month
      gw_crossection <- dplyr::filter(gwcode_changes, mydate == crossection_date)
      gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
      gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]

      # Set geometry to cell centroid
      sf::st_geometry(gw_crossection) <- NULL
      gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("x", "y"))
      sf::st_crs(gw_crossection) <- priogrid::prio_crs()

      # For each country where there have been changes, calculate the closest distance
      update_distances_in_these_countries <- unique(c(gw_crossection$gwcode, new_or_different_capital_location$gwcode))


      get_distances <- function(land_code){
         this_country <- cshp_crossection[which(cshp_crossection$gwcode == land_code), ]
         distances <- priogrid::get_closest_distance(gw_crossection[which(gw_crossection$gwcode==land_code),], this_country)
         return(distances)
      }

      distances_cross <- lapply(update_distances_in_these_countries, get_distances)

      gw_crossection$capdist <- NA_real_
      for(i in 1:length(update_distances_in_these_countries)){
         gw_crossection[gw_crossection$gwcode==update_distances_in_these_countries[i],"capdist"] <- distances_cross[[i]]
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
   pg <- priogrid::prio_blank_grid()
   pg_poly <- pg %>% spex::polygonize()
   gwcode_changes <- dplyr::left_join(gwcode_changes, pg_poly, by = "pgid") %>% sf::st_as_sf()

   cshp <- priogrid::monthly_cshp(input_folder)
   sf::st_geometry(cshp) <- NULL
   cshp <- sf::st_as_sf(cshp, coords = c("caplong", "caplat"))
   sf::st_crs(cshp) <- priogrid::prio_crs()

   changed_areas <- priogrid::gen_changed_areas(input_folder)
   dates_with_changes <- lubridate::ymd((names(changed_areas)))

   capdist <- lapply(changed_areas, calc_crossection, dates_with_changes) # calculate distances
   capdist <- priogrid::update_cells_iteratively(capdist, "capdist", changed_areas)
   return(capdist)
}
