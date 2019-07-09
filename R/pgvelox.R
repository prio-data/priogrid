

toVelox <- function(raster){
   raster %>%
      raster::stack() %>%
      velox::velox()
}
