
## Create RasterStack consisting of one RasterLayer for each year
# NB! If we are working with RasterBrick instead, just change raster::stack to raster::brick

# Prior to running, define variable and function for rasterization ("first", "last", "count" etc.)
# variable <- "variable_name"
# raster.fun <- "function"


yearly_stack <- function(data){
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
stack <- raster::stack(emp)
names(stack) <- paste0(variable, years)
return(stack)
}


