#' gen_irrig_sum 
#'
#' @param path The path to a folder containing AEI_EARTHSTAT_IR 1950-2005
#' @export
gen_irrig_sum <- function(path){
   gen_irrig(path,sum)
}

#' gen_irrig_sd 
#'
#' @param path The path to a folder containing AEI_EARTHSTAT_IR 1950-2005
#' @export
gen_irrig_sd <- function(path){
   gen_irrig(path,sd)
}

#' gen_irrig_min 
#'
#' @param path The path to a folder containing AEI_EARTHSTAT_IR 1950-2005
#' @export
gen_irrig_min <- function(path){
   gen_irrig(path,min)
}

#' gen_irrig_max 
#'
#' @param path The path to a folder containing AEI_EARTHSTAT_IR 1950-2005
#' @export
gen_irrig_max <- function(path){
   gen_irrig(path,max)
}

#' Generate irrigation variables
#' 
#' Generate variables measuring area equipped for irrigation
#' in each grid cell based on the Historical Irrigation Dataset v. 1.0.
#' 
#' @param hid AEI_EARTHSTAT_IR ascii files (1950-2005)
#' @param fun Function describing variable to be generated ("sum", "sd", "min", "max")

gen_irrig <- function(path, fun){
   years <- c(seq(1950,1980,10),
              seq(1985,2005,5))

   rasters <- get_irrig_data(path, years)
   hid <- do.call(raster::brick, rasters) 


   hid <- prio_aggregate_raster(hid, fun = fun)
  
   raster::extent(hid) <- priogrid::prio_extent()
   
   names(hid) <- paste0("irrig_", as.character(quote(fun)), "_" ,years)
  
   return(hid)
}

get_irrig_data <- function(path, years){
   base <- "AEI_EARTHSTAT_IR_"
   years <- c(seq(1950,1980,10),
              seq(1985,2005,5))
   paths <- file.path(path,paste0(base,years,".asc"))
   lapply(paths,raster::raster)
}
