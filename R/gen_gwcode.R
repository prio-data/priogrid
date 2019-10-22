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
      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]

      # Gids that have changed since last month
      gids_in_crossection <- sf::st_intersects(pgland, crossection)
      pg_crossection <- pgland[lengths(gids_in_crossection) > 0,]

      # Returns the inner join with the largest area owned in each pg cell that have changed since last month.
      gwcode <- sf::st_join(pg_crossection, cshp_crossection, join = sf::st_intersects, left = FALSE, largest = TRUE) %>%
         dplyr::select(layer, GWCODE)

      return(gwcode)
   }


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
      dplyr::mutate(GWEDAY = 1,
                    GWSDAY = 1) %>%
      dplyr::mutate(
         startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
         enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
      dplyr::mutate(
         date_interval = lubridate::interval(startdate, enddate)
      )

   all_months <- seq(min(cshp$startdate), max(cshp$enddate), by = "1 month")
   message("Find all areas and dates where there have been changes since last month.")
   unique_crossections <- parallel::mclapply(all_months, compare_crossection, cshp, mc.cores = numCores)
   unique_crossections[sapply(unique_crossections, is.null)] <- NULL

   pg <- priogrid::prio_blank_grid()

   # ca 22 minutter på 1 kjerne
   message("Calculate gwcode-ownership for each cell, for each month where ownership changes somewhere in the world.")
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

get_closest_distance <- function(points, features){
   nearest_feature <- sf::st_nearest_feature(points, features)
   nearest_point <- sf::st_nearest_points(points, features[nearest_feature,], pairwise = TRUE)
   return(sf::st_length(nearest_point))
}


gen_dcoast <- function(fname, quiet = TRUE){

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

   coastline <- rnaturalearth::ne_coastline(returnclass = "sf")


   pgland$dcoast <- priogrid::get_closest_distance(pgland, coastline)
   pg <- priogrid::prio_blank_grid()
   dcoast <- raster::rasterize(pgland, pg, field = "dcoast")

   return(dcoast)
}

gen_bdists <- function(fname, quiet = TRUE){

   gen_bdist1_month <- function(crossection_date){
      #bdist1: distance in km from the centroid to the border of the nearest land-contiguous neighboring country.

      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]
      land_intersections <- sf::st_intersects(cshp_cross)

      # Drop self-reference
      for(i in 1:length(land_intersections)){
         land_intersections[i][[1]] <- land_intersections[i][[1]][land_intersections[i][[1]] != i]
      }

      # only measure distance between the cell and the land contigous neighbors for that country.
      gwmonth <- subset(gwcode, which(crossection_date %in% all_months))
      gwmonth <- raster::rasterToPoints(gwmonth)
      gwmonth <- dplyr::tibble("gwcode" = gwmonth[,3], "lon" = gwmonth[,1], "lat" = gwmonth[,2])
      gwmonth <- sf::st_as_sf(gwmonth, coords = c("lon", "lat"))
      sf::st_crs(gwmonth) <- sf::st_crs(4326)
      gwmonth <- st_join(pgland, gwmonth)
      gwmonth <- na.omit(gwmonth) # drop cells without any country


      # Iterate over each country, and calculate the nearest distance to contiguos neighbor for all gis in country.
      gwmonth$bdist1 <- NA
      for(land_code in unique(gwmonth$gwcode)){
         land_intersection_index <- which(cshp_cross$GWCODE == land_code)
         neighbor_index <- land_intersections[land_intersection_index][[1]]
         neighbors <- cshp_cross[neighbor_index, ]
         gwmonth[gwmonth$gwcode==land_code,"bdist1"] <- priogrid::get_closest_distance(
                                    dplyr::filter(gwmonth, gwcode == land_code), neighbors)
      }

      # Rasterize results and return
      pg <- priogrid::prio_blank_grid()
      bdist1 <- raster::rasterize(gwmonth, pg, field = "bdist1")
      return(bdist1)
   }

   gen_bdist2_month <- function(crossection_date){
      #bdist2: distance in km from the centroid to the border of the nearest neighboring country.
      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

      # only measure distance between the cell and the land contigous neighbors for that country.
      gwmonth <- subset(gwcode, which(crossection_date %in% all_months))
      gwmonth <- raster::rasterToPoints(gwmonth)
      gwmonth <- dplyr::tibble("gwcode" = gwmonth[,3], "lon" = gwmonth[,1], "lat" = gwmonth[,2])
      gwmonth <- sf::st_as_sf(gwmonth, coords = c("lon", "lat"))
      sf::st_crs(gwmonth) <- sf::st_crs(4326)
      gwmonth <- st_join(pgland, gwmonth)
      gwmonth <- na.omit(gwmonth) # drop cells without any country

      # Iterate over each country, and calculate the nearest distance to nearest neighbor for all gis in country.
      gwmonth$bdist2 <- NA
      for(land_code in unique(gwmonth$gwcode)){
         neighbors <- cshp_cross[which(cshp_cross$GWCODE != land_code), ]
         gwmonth[gwmonth$gwcode==land_code,"bdist2"] <- priogrid::get_closest_distance(
            dplyr::filter(gwmonth, gwcode == land_code), neighbors)
      }

      # Rasterize results and return
      pg <- priogrid::prio_blank_grid()
      bdist2 <- raster::rasterize(gwmonth, pg, field = "bdist2")
      return(bdist2)
   }

   gen_bdist3_month <- function(crossection_date){
      #bdist3: distance in km from the centroid to the territorial outline of the country the cell belongs to.

      cshp_cross <- cshp[crossection_date %within% cshp$date_interval,]

      # only measure distance between the cell and the land contigous neighbors for that country.
      gwmonth <- subset(gwcode, which(crossection_date %in% all_months))
      gwmonth <- raster::rasterToPoints(gwmonth)
      gwmonth <- dplyr::tibble("gwcode" = gwmonth[,3], "lon" = gwmonth[,1], "lat" = gwmonth[,2])
      gwmonth <- sf::st_as_sf(gwmonth, coords = c("lon", "lat"))
      sf::st_crs(gwmonth) <- sf::st_crs(4326)
      gwmonth <- st_join(pgland, gwmonth)
      gwmonth <- na.omit(gwmonth) # drop cells without any country

      # Iterate over each country, and calculate the nearest distance to nearest neighbor for all gis in country.
      gwmonth$bdist3 <- NA
      for(land_code in unique(gwmonth$gwcode)){
         this_country <- cshp_cross[which(cshp_cross$GWCODE == land_code), ]
         gwmonth[gwmonth$gwcode==land_code,"bdist3"] <- priogrid::get_closest_distance(
            dplyr::filter(gwmonth, gwcode == land_code), this_country)
      }

      # Rasterize results and return
      pg <- priogrid::prio_blank_grid()
      bdist3 <- raster::rasterize(gwmonth, pg, field = "bdist3")
      return(bdist3)

   }



   message("Get gwcodes for each cell from file.")
   gwcode_file <- paste0(output_folder, "gwcode.rds")
   if(!is.null(output_folder) &  file.exists(gwcode_file)){
      gwcode <- readRDS(gwcode_file)
   } else{
      return(paste(gwcode_file, "does not exist. Please calculate gwcode first."))
   }

   message("Get the set of grid cells that intersect with land from file.")
   pgland_file <- paste0(output_folder, "pgland.rds")
   if(!is.null(output_folder) &  file.exists(pgland_file)){
      pgland <- readRDS(pgland_file)
   } else{
      break(paste(pgland_file, "does not exist. Please calculate pgland first."))
   }

   pgland <- raster::rasterToPoints(pgland)
   pgland <- dplyr::tibble("gid" = pgland[,3], "lon" = pgland[,1], "lat" = pgland[,2])
   pgland <- sf::st_as_sf(pgland, coords = c("lon", "lat"))
   sf::st_crs(pgland) <- sf::st_crs(4326)


   message("Loading cshapes")
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

   message("bdist1: distance in km from the centroid to the border of the nearest land-contiguous neighboring country.")
   bdist1 <- parallel::mclapply(all_months, gen_bdist1_month, mc.cores = numCores)
   bdist1 <- raster::stack(bdist1)
   names(bdist1) <- all_months

   message("bdist2: distance in km from the centroid to the border of the nearest neighboring country.")
   bdist2 <- parallel::mclapply(all_months, gen_bdist2_month, mc.cores = numCores)
   bdist2 <- raster::stack(bdist2)
   names(bdist2) <- all_months

   message("bdist3: distance in km from the centroid to the territorial outline of the country the cell belongs to.")
   bdist3 <- parallel::mclapply(all_months, gen_bdist3_month, mc.cores = numCores)
   bdist3 <- raster::stack(bdist3)
   names(bdist3) <- all_months


   return(list(bdist1, bdist2, bdist3))
}


#message("capdist: distance in km from the cell centroid to the national capital in the country the cell belongs to")
#message("seadist: distance in km to the nearest ocean")
#message("riverdist: distance in km to the nearest major river")

