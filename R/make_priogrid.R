

#' Make PRIOGrid from raw data
#'
#' This is the "master function" that turns raw data
#' into PRIOGrid.
#' Not functional, only for illustration.
#' @param input_folder Location of folder containing 
#' folders of raw data
#' @param output_folder Output folder

make_pg <- function(input_folder, output_folder){
   intermediate_folder <- tempdir()

   priogrid::make_gwcode(paste0(input_folder,'/cshapes/cshapes.shp'),
                         paste0(intermediate_folder,'/gwcode.rds'))
   #priogrid::make_other_variables(...)
   
   #for(f in list.files(intermediate_folder){
   #   ...stitch it all together... 
   #}
}
