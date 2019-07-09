


make_gwno <- function(cshp, date_override = NULL, detail = NULL, ncores = 1){
   tictoc::tic('Doing the whole thing')

   blank_grid <- priogrid::prio_blank_grid()
   full_landgrid <- priogrid::landgrid(cshp, blank = blank_grid) 

   #borders <- st_simplify(cshp,dTolerance = 1) %>%
   #   st_boundary() %>%
   #   st_buffer(1) %>%
   #   st_simplify(dTolerance = 1)
   #borderVelox <- velox(blank_grid)
   #borderVelox$rasterize(borders)

   getGwRaster <- function(date, cshp, raster, detail){
      tictoc::tic(glue::glue('Doing {strftime(date,"%d %b %Y")}'))
      crossection <- cshp[date %within% cshp$date_interval,]
      baseVelox <- velox::velox(raster)
      baseVelox$rasterize(crossection, field = 'GWCODE', band = 1, background = NA)
      baseRaster <- baseVelox$as.RasterStack()[[1]]

      if(!is.null(detail)){
         tictoc::tic('Detailing')
         fineVelox <- velox::velox(raster::disaggregate(raster, detail))
         fineVelox$rasterize(crossection, field = 'GWCODE', band = 1, background = NA)
         fineRaster <- fineVelox$as.RasterStack()[[1]]
         fineRaster <- raster::aggregate(fineRaster, detail, fun = raster::modal)
         diff <- baseRaster != fineRaster
         print(glue::glue('Diff: {sum(values(diff), na.rm = TRUE)}'))
         #baseRaster[diff] <- fineRaster
         tictoc::toc()
      }

      tictoc::toc()
      stck <- raster::stack(baseRaster,fineRaster,diff)
      names(stck) <- c('base','fine','diff')
      stck

   }

   cshp <- cshp %>%
     dplyr::filter(GWCODE != -1) %>%
     dplyr::mutate(
       startdate = lubridate::ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
       enddate = lubridate::ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-")),
       date_interval = lubridate::interval(startdate, enddate)
     )

   unique_dates <- unique(c(cshp$startdate, cshp$enddate))
    
   if(!is.null(date_override)){
      print(glue::glue('Overriding date'))
      unique_dates <- date_override
   }

   print(glue::glue('Applying to {length(unique_dates)} dates!'))

   unique_dates <- sort(unique_dates)
   res <- parallel::mclapply(unique_dates, getGwRaster,
          cshp = cshp,
          raster = full_landgrid,
          mc.cores = ncores,
          detail = detail)
   tictoc::toc()
   return(res)
}
