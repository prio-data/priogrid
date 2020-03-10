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
   gwcode <- gen_gwcode_month(fname, numCores, quiet)
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
   cshp <- sf::read_sf(file.path(input_folder, "cshapes.shp"))

   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1)

   pg <- priogrid::prio_blank_grid()
   pg_poly <- pg %>% spex::polygonize()

   land_gids <- sf::st_intersects(pg_poly, cshp)
   pgland <- pg_poly[lengths(land_gids)> 0,]
   pgland <- dplyr::tibble("pgid" = pgland$pgid, "pgland" = 1L)
   return(pgland)
}

gen_landarea <- function(fname, output_folder, quiet = TRUE){
   message("Get the set of grid cells that intersect with land from file.")
   pgland_file <- paste0(output_folder, "pgland.rds")
   if(!is.null(output_folder) &  file.exists(pgland_file)){
      pgland <- readRDS(pgland_file)
   } else{
      break(paste(pgland_file, "does not exist. Please calculate pgland first."))
   }
   pgland <- spex::polygonize(pgland)

   cshp <- sf::st_read(fname, quiet = quiet)
   cshp <- cshp %>%
      dplyr::filter(GWCODE != -1, GWEYEAR == max(GWEYEAR))

   land_polygons <- sf::st_intersection(pgland, cshp)
   land_polygons$pgarea <- sf::st_area(land_polygons)

   sf::st_geometry(land_polygons) <- NULL

   land_polygons <- dplyr::group_by(land_polygons, layer) %>%
     dplyr::summarise(pgarea = sum(pgarea, na.rm = T))
   #...
   pgland <- dplyr::left_join(pgland, land_polygons, by = "layer")
   pgland <- sf::st_centroid(pgland)

   pg <- priogrid::prio_blank_grid()
   landarea <- raster::rasterize(pgland, pg, field = "pgarea")
   return(landarea)
}

#' gen_changed_areas
#'
#' Takes the weidmann cshapes data set and returns a
#' list of sf dataframes that corresponds to the areas where borders changed from one month to the next.
#'
#' @param fname File path to Weidmann cshapes data
#' @param numCores Number of cores to use in calculation. Windows-users can only use 1. Using parallel::mclapply.
#' @param quiet Whether or not sf::st_ functions should print warnings.
gen_changed_areas <- function(fname, numCores = 1, quiet = TRUE){
   compare_crossection <- function(crossection_date, cshp){
      message(crossection_date)
      if(crossection_date - lubridate::month(1) < min(cshp$startdate)){
         changed_areas <- cshp[crossection_date %within% cshp$date_interval,]
         changed_areas$crossection_date <- crossection_date
      } else {
         past_crossection <- cshp[(crossection_date - lubridate::month(1)) %within% cshp$date_interval,]
         cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
         new_changes <- lengths(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0)) == 0

         if(any(new_changes)){
            cshp_crossection$changes <- new_changes
            #past_crossection$changes <- lengths(sf::st_equals_exact(past_crossection, cshp_crossection, par = 0)) == 0

            cshp_crossection <- dplyr::filter(cshp_crossection, changes)
            #past_crossection <- dplyr::filter(past_crossection, changes)

            #changed_areas <- rbind(cshp_crossection, past_crossection)
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

   message("Loading cshapes")
   cshp <- sf::st_read(fname, quiet = quiet)

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
#      dplyr::mutate(
#         enddate = enddate - lubridate::days(1) # End up until, but not including
#      ) %>%
      dplyr::mutate(
         date_interval = lubridate::interval(startdate, enddate)
      )

   all_months <- seq(min(cshp$startdate), max(cshp$enddate), by = "1 month")
   message("Find all areas and dates where there have been changes since last month.")
   changed_areas <- parallel::mclapply(all_months, compare_crossection, cshp, mc.cores = numCores)
   changed_areas[sapply(changed_areas, is.null)] <- NULL

   crossection_dates <- sapply(changed_areas, function(x) unique(x$crossection_date))
   crossection_dates <- as.character(as.Date(crossection_dates, origin = as.Date("1970-1-1")))

   names(changed_areas) <- crossection_dates


   return(changed_areas)
}

#' gen_gwcode_month
#'
#' Takes the weidmann cshapes data set and returns a
#' rasterstack that encodes the country ownership of a cell as it looked like on the 1st each month.
#' The rasterstack only includes the months where there was a change from one month to the next.
#'
#' @param fname File path to Weidmann cshapes data
#' @param numCores Number of cores to use in calculation. Windows-users can only use 1. Using parallel::mclapply.
#' @param quiet Whether or not sf::st_ functions should print warnings.
gen_gwcode_month <- function(fname, numCores = 1, quiet = quiet, output_folder = NULL){
   calc_crossection <- function(crossection){
      crossection_date <- unique(crossection$crossection_date)
      if(length(crossection_date) != 1){
         message("Non-unique crossection date!")
      }
      message(crossection_date)

      # Global crossection of cshapes used to determine correct gwcode
      past_crossection <- cshp[(crossection_date - lubridate::month(1)) %within% cshp$date_interval,]
      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
      new_changes <- lengths(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0)) == 0
      cshp_crossection$changes <- new_changes
      cshp_crossection <- dplyr::filter(cshp_crossection, changes)

      # Gids that have changed since last month
      gids_in_crossection <- sf::st_intersects(pgland, crossection)
      pg_crossection <- pgland[lengths(gids_in_crossection) > 0,]

      # Returns the inner join with the largest area owned in each pg cell that have changed since last month.
      gwcode <- sf::st_join(pg_crossection, cshp_crossection, join = sf::st_intersects, left = FALSE, largest = TRUE) %>%
         dplyr::select(layer, GWCODE)

      return(gwcode)
   }

   message("Get the countries where the border changed from one month to the next.")
   changed_areas_file <- paste0(output_folder, "changed_areas.rds")
   if(!is.null(output_folder) &  file.exists(changed_areas_file)){
      changed_areas <- readRDS(changed_areas_file)
   } else{
      break(paste(changed_areas_file, "does not exist. Please calculate changed_areas first."))
   }

   message("Get the set of grid cells that intersect with land from file.")
   pgland_file <- paste0(output_folder, "pgland.rds")
   if(!is.null(output_folder) &  file.exists(pgland_file)){
      pgland <- readRDS(pgland_file)
   } else{
      break(paste(pgland_file, "does not exist. Please calculate pgland first."))
   }
   pgland <- spex::polygonize(pgland)

   message("Loading cshapes")
   cshp <- sf::st_read(fname, quiet = quiet)

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
#       enddate = enddate - lubridate::days(1) # End up until, but not including
#     ) %>%
     dplyr::mutate(
         date_interval = lubridate::interval(startdate, enddate)
      )

   pg <- priogrid::prio_blank_grid()

   # ca 22 minutter på 1 kjerne
   message("Calculate gwcode-ownership for each cell, for each month where ownership changes somewhere in the world.")
   gwcodes <- parallel::mclapply(changed_areas, calc_crossection, mc.cores = numCores)

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

   crossection_dates <- sapply(changed_areas, function(x) unique(x$crossection_date))
   crossection_dates <- as.character(as.Date(crossection_dates, origin = as.Date("1970-1-1")))

   names(gwcode) <- crossection_dates
   return(gwcode)
}

get_closest_distance <- function(points, features, check_dateline=TRUE){
   nearest_feature <- sf::st_nearest_feature(points, features)
   nearest_point <- sf::st_nearest_points(points, features[nearest_feature,], pairwise = TRUE)

   distances <- sf::st_length(nearest_point)

   if(check_dateline){
      pointsT <- st_transform(points,"+proj=longlat +datum=WGS84 +pm=180") %>% st_wrap_dateline()
      featuresT <- st_transform(features,"+proj=longlat +datum=WGS84 +pm=180") %>% st_wrap_dateline()
      nearest_featureT <- sf::st_nearest_feature(pointsT, featuresT)
      nearest_pointT <- sf::st_nearest_points(pointsT, featuresT[nearest_featureT,], pairwise = TRUE)
      distancesT <- sf::st_length(nearest_pointT)
      distances <- apply(cbind(distances, distancesT), 1, min)
   }
   return(distances)
}


gen_dcoast <- function(fname, output_folder, quiet = TRUE){

   message("Get the set of grid cells that intersect with land from file.")
   pgland_file <- paste0(output_folder, "pgland.rds")
   if(!is.null(output_folder) &  file.exists(pgland_file)){
      pgland <- readRDS(pgland_file)
   } else{
      return(paste(pgland_file, "does not exist. Please calculate pgland first."))
   }

   pgland <- raster::rasterToPoints(pgland)
   pgland <- dplyr::tibble("gid" = pgland[,3], "lon" = pgland[,1], "lat" = pgland[,2])
   pgland <- sf::st_as_sf(pgland, coords = c("lon", "lat"))
   sf::st_crs(pgland) <- sf::st_crs(4326)

   # We use GSHHG, with the "low" resolution (GSHHS_l_L1.shp)
   coastline <- sf::st_read(fname, quiet = quiet)
   coastline <- sf::st_boundary(coastline)


   pgland$dcoast <- priogrid::get_closest_distance(pgland, coastline)
   pg <- priogrid::prio_blank_grid()
   dcoast <- raster::rasterize(pgland, pg, field = "dcoast")

   return(dcoast)
}

gen_driver <- function(fnames, output_folder, quiet = TRUE){
   river1 <- fnames[1]
   river2 <- fnames[2]
   river3 <- fnames[3]
   lakes <- fnames[4]


   #river1 <- "C:/Users/jonves/data/priogrid_data/gshhg/data/WDBII_shp/l/WDBII_river_l_L01.shp"
   #river2 <- "C:/Users/jonves/data/priogrid_data/gshhg/data/WDBII_shp/l/WDBII_river_l_L02.shp"
   #river3 <- "C:/Users/jonves/data/priogrid_data/gshhg/data/WDBII_shp/l/WDBII_river_l_L03.shp"
   #lakes <- "C:/Users/jonves/data/priogrid_data/gshhg/data/GSHHS_shp/l/GSHHS_l_L2.shp"

   river1 <- sf::st_read(river1, quiet = T)
   river2 <- sf::st_read(river2, quiet = T)
   river3 <- sf::st_read(river3, quiet = T)
   lakes <- sf::st_read(lakes, quiet = T)
   lakes <- dplyr::filter(lakes, area >= 1000)

   lakes_and_rivers <- rbind(dplyr::select(river1, geometry),
                             dplyr::select(river2, geometry),
                             dplyr::select(river3, geometry),
                             dplyr::select(lakes, geometry))

   message("Get the set of grid cells that intersect with land from file.")
   pgland_file <- paste0(output_folder, "pgland.rds")
   if(!is.null(output_folder) &  file.exists(pgland_file)){
      pgland <- readRDS(pgland_file)
   } else{
      return(paste(pgland_file, "does not exist. Please calculate pgland first."))
   }

   pgland <- raster::rasterToPoints(pgland)
   pgland <- dplyr::tibble("gid" = pgland[,3], "lon" = pgland[,1], "lat" = pgland[,2])
   pgland <- sf::st_as_sf(pgland, coords = c("lon", "lat"))
   sf::st_crs(pgland) <- sf::st_crs(4326)
   pgland$driver <- priogrid::get_closest_distance(pgland, lakes_and_rivers)

   pg <- priogrid::prio_blank_grid()
   driver <- raster::rasterize(pgland, pg, field = "driver")

   return(driver)
}

gen_bdist1_month <- function(crossection, gwcode, cshp, numCores = 1){
  #bdist1: distance in km from the centroid to the border of the nearest land-contiguous neighboring country.
  crossection_date <- unique(crossection$crossection_date)
  message(crossection_date)

  cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]
  land_intersections <- sf::st_intersects(cshp_cross)

  # Drop self-reference
  for(i in 1:length(land_intersections)){
    land_intersections[i][[1]] <- land_intersections[i][[1]][land_intersections[i][[1]] != i]
  }

  months_in_gwcode <- lubridate::ymd(sub("X", "", names(gwcode)))
  # only measure distance between the cell and the land contigous neighbors for that country.
  gw_crossection <- raster::subset(gwcode, which(months_in_gwcode %in% crossection_date))
  gw_crossection <- raster::rasterToPoints(gw_crossection)
  gw_crossection <- dplyr::tibble("gwcode" = gw_crossection[,3], "lon" = gw_crossection[,1], "lat" = gw_crossection[,2])
  gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("lon", "lat"))
  sf::st_crs(gw_crossection) <- sf::st_crs(4326)

  #gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
  #gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]

  # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
  land_codes <- unique(gw_crossection$gwcode)
  get_distances <- function(land_code){
     land_intersection_index <- which(cshp_cross$GWCODE == land_code)
     neighbor_index <- land_intersections[land_intersection_index][[1]]
     neighbors <- cshp_cross[neighbor_index, ]
     return(priogrid::get_closest_distance(dplyr::filter(gw_crossection, gwcode == land_code), neighbors))
  }
  distances_cross <- parallel::mclapply(land_codes, get_distances, mc.cores = numCores)

  gw_crossection$bdist1 <- NA
  for(i in 1:length(land_codes)){
    gw_crossection[gw_crossection$gwcode==land_codes[i],"bdist1"] <- distances_cross[[i]]
  }

  # Rasterize results and return
  pg <- priogrid::prio_blank_grid()
  bdist1 <- raster::rasterize(gw_crossection, pg, field = "bdist1")

  return(bdist1)
}

gen_bdist2_month <- function(crossection, gwcode, cshp, numCores = 1){
  #bdist2: distance in km from the centroid to the border of the nearest neighboring country.
  crossection_date <- unique(crossection$crossection_date)
  cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

  months_in_gwcode <- lubridate::ymd(sub("X", "", names(gwcode)))
  # only measure distance between the cell and the land contigous neighbors for that country.
  gw_crossection <- raster::subset(gwcode, which(months_in_gwcode %in% crossection_date))
  gw_crossection <- raster::rasterToPoints(gw_crossection)
  gw_crossection <- dplyr::tibble("gwcode" = gw_crossection[,3], "lon" = gw_crossection[,1], "lat" = gw_crossection[,2])
  gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("lon", "lat"))
  sf::st_crs(gw_crossection) <- sf::st_crs(4326)

  #gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
  #gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]

  # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
  land_codes <- unique(gw_crossection$gwcode)
  get_distances <- function(land_code){
     neighbors <- cshp_cross[which(cshp_cross$GWCODE != land_code), ]
     return(priogrid::get_closest_distance(dplyr::filter(gw_crossection, gwcode == land_code), neighbors))
  }
  distances_cross <- parallel::mclapply(land_codes, get_distances, mc.cores = numCores)

  gw_crossection$bdist2 <- NA
  for(i in 1:length(land_codes)){
     gw_crossection[gw_crossection$gwcode==land_codes[i],"bdist2"] <- distances_cross[[i]]
  }

  # Rasterize results and return
  pg <- priogrid::prio_blank_grid()
  bdist2 <- raster::rasterize(gw_crossection, pg, field = "bdist2")
  return(bdist2)
}

gen_bdist3_month <- function(crossection, gwcode, cshp){
  #bdist3: distance in km from the centroid to the territorial outline of the country the cell belongs to.
  crossection_date <- unique(crossection$crossection_date)
  cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

  months_in_gwcode <- lubridate::ymd(sub("X", "", names(gwcode)))
  # only measure distance between the cell and the land contigous neighbors for that country.
  gw_crossection <- raster::subset(gwcode, which(months_in_gwcode %in% crossection_date))
  gw_crossection <- raster::rasterToPoints(gw_crossection)
  gw_crossection <- dplyr::tibble("gwcode" = gw_crossection[,3], "lon" = gw_crossection[,1], "lat" = gw_crossection[,2])
  gw_crossection <- sf::st_as_sf(gw_crossection, coords = c("lon", "lat"))
  sf::st_crs(gw_crossection) <- sf::st_crs(4326)

  gids_in_crossection <- sf::st_intersects(gw_crossection, crossection)
  gw_crossection <- gw_crossection[lengths(gids_in_crossection) > 0,]

  # Iterate over each country, and calculate distance in km from the centroid to the territorial outline of the country the cell belongs to.
  land_codes <- unique(gw_crossection$gwcode)
  gw_crossection$bdist2 <- NA

  for(i in 1:length(land_codes)){
     this_country <- cshp_cross[which(cshp_cross$GWCODE == land_codes[i]), ]
     centroid_within_country <- sf::st_within(dplyr::filter(gw_crossection, gwcode == land_codes[i]), this_country)
     this_country <- sf::st_boundary(this_country)
     gw_crossection$cwc <- NA
     gw_crossection$cwc[gw_crossection$gwcode == land_codes[i]] <- lengths(centroid_within_country) > 0

     # Cells belonging to a country, but with a centroid outside the border will have 0 distance to the outline of the country.
     gw_crossection[which(gw_crossection$gwcode==land_codes[i] & gw_crossection$cwc == FALSE), "bdist3"] <- 0

     gw_crossection[which(gw_crossection$gwcode==land_codes[i] & gw_crossection$cwc == TRUE),"bdist3"] <- priogrid::get_closest_distance(
        gw_crossection[which(gw_crossection$gwcode==land_codes[i] & gw_crossection$cwc == TRUE),], this_country)

  }

  # Rasterize results and return
  pg <- priogrid::prio_blank_grid()
  bdist3 <- raster::rasterize(gw_crossection, pg, field = "bdist3")
  return(bdist3)
}


gen_bdists <- function(fname, distance_type, output_folder, numCores = 1, quiet = TRUE){

  gwcode <- priogrid::get_rds_file("gwcode_month.rds", output_folder = output_folder)
  changed_areas <- priogrid::get_rds_file("changed_areas.rds", output_folder = output_folder)
  cshp <- sf::st_read(fname, quiet = quiet)

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
      # This causes error for Indonesia 1976-06-01. Check.
      #     dplyr::mutate(
      #       enddate = enddate - lubridate::days(1) # End up until, but not including
      #     ) %>%
      dplyr::mutate(
       date_interval = lubridate::interval(startdate, enddate)
      )

   if(distance_type == 1){
     message("bdist1: distance in km from the centroid to the border of the nearest land-contiguous neighboring country.")
     bdist <- lapply(changed_areas, priogrid::gen_bdist1_month, gwcode = gwcode, cshp = cshp, numCores = numCores)
   } else if (distance_type == 2){
     message("bdist2: distance in km from the centroid to the border of the nearest neighboring country.")
     bdist <- lapply(changed_areas, priogrid::gen_bdist2_month, gwcode = gwcode, cshp = cshp, numCores = numCores)
   } else if (distance_type == 3){
     message("bdist3: distance in km from the centroid to the territorial outline of the country the cell belongs to.")
     bdist <- parallel::mclapply(changed_areas, priogrid::gen_bdist3_month, gwcode = gwcode, cshp = cshp, mc.cores = numCores)
   } else {
     return("distance_type must be 1, 2 or 3.")
   }

   pg <- priogrid::prio_blank_grid()
   # Update classification scheme iteratively. Add the values that are not missing to the current raster.
   rasters <- list()
   i <- 1
   current_raster <- pg
   current_raster[] <- NA
   for(j in 1:length(bdist)){
     dc <- bdist[[j]]

     current_raster[!is.na(dc)] <- dc[!is.na(dc)]
     rasters[[i]] <- current_raster
     i <- i + 1
   }


   bdist <- raster::stack(rasters)
   names(bdist) <- names(changed_areas)

   return(bdist)
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
   gwcode_file <- paste0(output_folder, "gwcode_month.rds")
   if(!is.null(output_folder) &  file.exists(gwcode_file)){
      gwcode <- readRDS(gwcode_file)
   } else{
      return(paste(gwcode_file, "does not exist. Please calculate gwcode_month first."))
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
