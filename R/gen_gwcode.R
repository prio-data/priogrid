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

   unique_dates <- unique(c(cshapes$startdate, cshapes$enddate))
   unique_dates <- sort(unique_dates)

   if(partial){
      whichdates <- sample(c(TRUE,FALSE), size = length(unique_dates), 
                           replace = TRUE, prob = c(0.1,0.9))
      whichdates[1] <- TRUE
      whichdates[length(whichdates)] <- TRUE
      print(glue::glue('Using {sum(whichdates)}/{length(unique_dates)} dates'))
      unique_dates <- unique_dates[whichdates]
   }

   base <- prio_blank_grid()
   out <- list()

   i <- 1
   tictoc::tic('Looping over dates')
   for(specdate in unique_dates){
      specdate <- as.Date(specdate,as.Date('1970-01-01'))
      print(glue::glue('Doing {specdate}'))

      countries <- which(cshapes$startdate == specdate | cshapes$enddate == specdate)
      cshapes_cs <- cshapes[countries,]

      tictoc::tic('Time spent')

      res <- rasterize_worldtiles(cshapes_cs, base,
                   rasterize_tile, ...)

      if(length(res) > 1){
         res <- do.call(raster::merge,res)
      } else {
         res <- res[[1]]
      }

      if(i > 1){
         res <- raster::merge(res,prev)
      } prev <- res

      out[[strftime(specdate)]] <- res

      i <- i + 1
      tictoc::toc()
   }
   tictoc::toc()
   out
}

rasterize_worldtiles <- function(vectors, raster, fun, 
                         vectorsOfInterest = NULL, 
                         subdiv = subdiv, ncore = 1, ...){

   tiles <- raster::raster(nrow = subdiv, ncol = subdiv * 2,
                   ext = raster::extent(raster),
                   crs = raster::crs(raster))

   polytiles <- spex::polygonize(tiles)
   print(glue::glue('Using {nrow(polytiles)} tiles'))

   res <- parallel::mclapply(1:nrow(polytiles), mc.cores = ncore,
            function(tilenumber){

      intersections <- suppressMessages(sf::st_intersects(polytiles[tilenumber,], 
                                                          vectors))

      relevantvector <- vectors[intersections[[1]],]
      relevantraster <- raster::crop(raster,polytiles[tilenumber,])
      
      tile_info <- list(intersections = intersections[[1]]) 

      fun(relevantvector, relevantraster, tile_info, detail)
   })

   res[!is.na(res)]
}

rasterize_tile <- function(vec,ras, tile_info, detail = detail){
   if(nrow(vec) > 0){
      if(length(tile_info$intersections) > 1){
         gigaras <- raster::disaggregate(ras,detail)
         vx <- velox::velox(gigaras)

         vx$rasterize(vec, 'GWCODE', 1, NA)
         ras <- vx$as.RasterStack()[[1]]
         raster::aggregate(ras, detail, fun = raster::modal)

      } else {
         vx <- velox::velox(ras)
         vx$rasterize(vec, 'GWCODE', 1, NA)
         vx$as.RasterStack()[[1]]
      }

   } else {
      # Ocean tiles
      raster::values(ras) <- NA
      ras
   }

}
