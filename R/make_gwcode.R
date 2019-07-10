
mk_gwcode <- function(cshapes, partial = TRUE, ...){

   unique_dates <- unique(c(cshapes$startdate, cshapes$enddate))
   unique_dates <- sort(unique_dates)

   if(partial){
      whichdates <- sample(c(TRUE,FALSE), size = length(unique_dates), 
                           replace = TRUE, prob = c(0.1,0.9))
      whichdates[1] <- TRUE
      whichdates[length(whichdates)] <- TRUE
      unique_dates <- unique_dates[whichdates]
   }

   base <- prio_blank_grid()

   out <- list()

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
         res <- do.call(merge,res)
      } else {
         res <- res[[1]]
      }

      out[[strftime(specdate)]] <- res

      tictoc::toc()
   }

   tictoc::toc()
   out
}

rasterize_worldtiles <- function(vectors, raster, fun, 
                         vectorsOfInterest = NULL, 
                         subdiv = 4, ncore = 1, ...){

   #if(is.null(vectorsOfInterest)){
   #   # Only do function for relevant vectors,
   #   # unless nothing was specified.
   #   vectorsOfInterest <- 1:nrow(vectors)
   #   print(glue::glue('Using all {length(vectorsOfInterest)} vectors'))
   #} else {
   #   print(glue::glue('Using {length(vectorsOfInterest)} vectors'))
   #}

   tiles <- raster::raster(nrow = subdiv, ncol = subdiv * 2,
                   ext = raster::extent(raster),
                   crs = raster::crs(raster))

   polytiles <- spex::polygonize(tiles)
   print(glue::glue('Using {nrow(polytiles)} tiles'))

   res <- parallel::mclapply(1:nrow(polytiles), mc.cores = ncore,
            function(tilenumber){

      intersections <- suppressMessages(sf::st_intersects(polytiles[tilenumber,], vectors))

      if(any(intersections[[1]] %in% vectorsOfInterest)){
         relevantvector <- vectors[intersections[[1]],]
         relevantraster <- raster::crop(raster,polytiles[tilenumber,])
         
         tile_info <- list(intersections = intersections[[1]]) 

         fun(relevantvector, relevantraster, tile_info,  ...)
      } else {
         NA
      }
   })

   res[!is.na(res)]
}

rasterize_tile <- function(vec,ras, tile_info, detail = 2){
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
