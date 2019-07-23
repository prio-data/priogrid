
## Create RasterBrick consisting of one RasterLayer for each year

# NOTE: This is fairly quick on smaller point data, but takes a lot of time on larger data. 
# Look into vectorized solution.

# Define variable and raster.fun as character string. 


yearly_brick <- function(data, variable, raster.fun){
years <- unique(data$year)
emp <- list()

for(i in 1:length(years)) {
  y <- years[i]
  if(sum(data$year == y) > 0)
    emp[[i]] <- raster::rasterize(data[which(data$year == y),],
                                  priogrid::prio_blank_grid(),
                                  field = variable,
                                  fun = raster.fun)
  }
brick <- raster::brick(emp)
names(brick) <- paste0(variable,"_", years)
return(brick)
}
