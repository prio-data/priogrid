
# Just some mock functions to do testing. 
# They might also be useful for defining spec for classes of functions
# See the gen_function

gen_myvar <- function(inpath){
   data <- readRDS(file.path(inpath,"dat.rds"))
   data <- rbind(data,data)
   data
}
