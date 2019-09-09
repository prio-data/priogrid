

#' Make PRIOGrid from raw data
#'
#' This is the "master function" that turns raw data
#' into PRIOGrid.
#' Not functional, only for illustration.
#' @param input_folder Location of folder containing
#' folders of raw data
#' @param output_folder Output folder

make_pg <- function(input_folder, output_folder, overwrite = FALSE){
   #intermediate_folder <- tempdir()

   priogrid::make_gwcode(paste0(input_folder,'/cshapes/data/cshapes.shp'),
                         paste0(output_folder,'/gwcode.rds'), overwrite = overwrite)
   priogrid::make_pop_gpw(paste0(input_folder,'/pop_gpw/data/gpw_v4_population_count_rev11_2pt5_min.nc'),
                          paste0(output_folder,'/pop_gpw.rds'), overwrite = overwrite)
   #priogrid::make_other_variables(...)

   #for(f in list.files(intermediate_folder){
   #   ...stitch it all together...
   #}
}

make_gwcode <- function(input_file, output_file, overwrite){
  if(!overwrite){
     if(file.exists(output_file)){
        return(paste("File", output_file, "already exists."))
     }
  }
  infile <- sf::st_read(input_file)
  rast <- gen_gwcode(infile)
  saveRDS(rast, file = output_file)
}

make_pop_gpw <- function(input_file, output_file){
  if(!overwrite){
     if(file.exists(output_file)){
        return(paste("File", output_file, "already exists."))
     }
  }
  rast <- gen_pop_gpw_c(infile)
  saveRDS(rast, file = output_file)
}
