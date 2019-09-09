#' gen_gwcode
#'
#' Takes the weidmann cshapes data set and returns a
#' list of rasterlayers for each _changed_ country-
#' border year. Arguments are passed to rasterize_tile and rasterize_worldtiles.
#' Useful arguments to specify
#'
#' @param cshapes weidmann cshapes data
#' @param partial Only do some of the dates? Useful for debugging.
#' @param subdiv How many tiles to consider, shortening computation
#' @param detail How much detail to use when rasterizing the polygons.

gen_gwcode <- function(cshapes, partial = FALSE, subdiv = 16, detail = 16){
   # made by Peder
   # Nonfunctional

   cshapes <- cshapes %>%
     dplyr::filter(GWCODE != -1) %>%
     dplyr::mutate(
       startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
       enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))) %>%
     dplyr::mutate(
       date_interval = lubridate::interval(startdate, enddate)
     )

   cshapes$id <- as.numeric(row.names(cshapes))

   # Figuring out tiles =============

   simpleWorld <- sf::st_simplify(cshapes,dTolerance = 1)
   simpleBorders <- sf::st_boundary(simpleWorld)

   tiles <- raster::raster(nrow = subdiv, ncol = subdiv * 2,
                           ext = priogrid::prio_extent(),
                           crs = priogrid::prio_crs()) %>%
      spex::polygonize()

   tiles$intersections <- sapply(sf::st_intersects(tiles,simpleWorld),unlist)

   tiles$boundarytile <- sapply(sf::st_intersects(tiles,simpleBorders),length) != 0
   tiles$bordertile <- sapply(sf::st_intersects(tiles,simpleBorders),length) > 1

   tiles$id <- as.numeric(row.names(tiles))

   hasintersect <- sapply(tiles$intersections, length) != 0
   tiles <- tiles[hasintersect,]

   # Looping! =======================

   years <- seq(lubridate::floor_date(min(cshapes$startdate),unit = 'year'),
                lubridate::floor_date(max(cshapes$enddate),unit = 'year'),
                by = 'years')

   base <- priogrid::prio_blank_grid()
   out <- list()
   done <- logical(nrow(cshapes))

   for(start_of_year in years){
      start_of_year <- as.Date(start_of_year,as.Date('1970-01-01'))
      print(glue::glue('Doing {start_of_year}'))

      current <- logical(length = nrow(cshapes))
      current[which(cshapes$startdate <= start_of_year)] <- TRUE
      current <- current & ! done
      exist <- current | done & !cshapes$enddate < start_of_year

      print(glue::glue('{sum(current)} countries to do in {start_of_year}'))
      print(glue::glue('{sum(exist)} countries exist in {start_of_year}'))

      current_countries <- cshapes[current,]
      existing_countries <- cshapes[exist,]

      current_tiles <- sapply(tiles$intersections,function(inters){
         any(current_countries$id %in% inters)
         })

      current_tiles <- tiles[current_tiles,]


      if(nrow(current_tiles) > 0){
         print(glue::glue('Doing {nrow(current_tiles)} tiles'))
         res <- rasterize_tilewise(current_tiles, current_countries, base, detail = detail)
      } else {
         res <- prio_blank_grid()
         values(res) <- NA
         print(glue::glue('Nothing to do!'))
      }

      out[[strftime(start_of_year)]] <- res

      done <- done | current
      print(glue::glue('Done with {sum(done)} countries'))
   }
   for(i in 1:length(out)){
      if(i > 1){
         out[[i]] <- raster::merge(out[[i - 1]],out[[i]])
      }
   }
   #out <- raster::stack(out)

   # Make sure that all years have same extent by resampling to nearest neighbor.
   out <- lapply(out, function(x) raster::resample(x, base, method = "ngb"))
   out <- raster::stack(out)
   return(out)
}

rasterize_tilewise <- function(tiles, countries, base, detail = 2){

   veloxRasterize <- function(r,vectors,detail = 1){
      if(detail > 1){
         r <- disaggregate(r,fact = detail)
      }
      vx <- velox::velox(r)
      vx$rasterize(vectors, field = 'GWCODE', band = 1,background = NA)
      r <- vx$as.RasterLayer()

      if(detail > 1){
         r <- aggregate(r,fact = detail,fun = raster::modal)
      }
      r
   }

   rasters <- list()
   for(i in 1:nrow(tiles)){

      tile <- tiles[i,]

      r <- crop(base,tiles[i,])

      curr_countries <- countries[countries$id %in% unlist(tile$intersections),]

      # Different levels of detail, ====
      # ================================
      if(tile$bordertile){
         curr_detail <- detail
      } else if(tile$boundarytile){
         curr_detail <- round(detail / 2)
         curr_detail <- 1
      } else {
         curr_detail <- 1
      }

      if(nrow(curr_countries) > 0){
         r <- veloxRasterize(r,curr_countries,curr_detail)
      }

      rasters[[length(rasters) + 1]] <- r
   }

   do.call(raster::merge,rasters)
}

