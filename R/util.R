
#' Old grid to raster
#' 
#' Converts the .csv PrioGrid v.1 into a raster.
#' Useful for testing equivalency.
#' @param timeseries The data frame contained in the timeseries version of PrioGrid
#' @param year The desired year. Currently only yields a single raster for a single year. 
#' @param variable A vector of variable[s] to return in a stack
#' @return A raster stack with rasters for each variable
#' @examples
#' prioTimeseries <- read.csv('timeseries.csv') 
#' prioStack <- refToRaster(prioTimeseries, year = 1993, variable = 'gwno')
#' plot(prioStack)

refToRaster <- function(timeseries, year = 1970, variable = 'gwno'){
   blank_grid <- priogrid::prio_blank_grid()
   rasterstack <- raster::stack(blank_grid)

   gid_cid_reference <- blank_grid %>%
      raster::as.data.frame() %>%
      tibble::as_tibble() %>%
      tibble::rownames_to_column('cid') %>%
      dplyr::rename(gid = layer)

   crossection <- timeseries[timeseries$year == year,]
   crossection <- crossection[,c('gid',variable)]

   crossection <- merge(gid_cid_reference, crossection, by = 'gid')
   crossection <- dplyr::arrange(crossection,as.numeric(cid))

   inBoth <- 1:length(rasterstack) %in% crossection$cid
   
   for(v in variable){
      rstr <- blank_grid

      raster::values(rstr) <- NA
      rstr[inBoth] <- crossection[[v]]
      names(rstr) <- v

      rasterstack <- raster::stack(rasterstack,rstr)
   }

   rasterstack
}
