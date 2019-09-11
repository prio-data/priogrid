

#' Make PRIOGrid from raw data
#'
#' This is the "master function" that turns raw data
#' into PRIOGrid.
#' Not functional, only for illustration.
#' @param input_folder Location of folder containing
#' folders of raw data
#' @param output_folder Output folder

make_pg <- function(input_folder, output_folder, overwrite = FALSE, yearly = TRUE, static = TRUE, seasonal = TRUE){
   #intermediate_folder <- tempdir()


   if(yearly){
      priogrid::make_gwcode(paste0(input_folder,'/cshapes/data/cshapes.shp'),
                            paste0(output_folder,'/gwcode.rds'), overwrite = overwrite)
      priogrid::make_pop_gpw(paste0(input_folder,'/pop_gpw/data/gpw_v4_population_count_rev11_2pt5_min.nc'),
                             paste0(output_folder,'/pop_gpw.rds'), overwrite = overwrite)

   }
   if(static){
      priogrid::make_grow_agg(paste0(input_folder, "/mirca/data/CELL_SPECIFIC_CROPPING_CALENDARS.TXT.gz"),
                              paste0(output_folder, "/mirca_grow_agg.rds"), overwrite = overwrite)
      priogrid::make_diamonds(paste0(input_folder,'/diamonds/data/DIADATA.shp'),
                              paste0(output_folder,'/diamonds.rds'), overwrite = overwrite)

   }
   if(seasonal){
      priogrid::make_growseas(paste0(input_folder, "/mirca/data/CELL_SPECIFIC_CROPPING_CALENDARS.TXT.gz"),
                              paste0(output_folder, "/mirca_growseas.rds"), overwrite = overwrite)
   }

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

make_pop_gpw <- function(input_file, output_file, overwrite){
  if(!overwrite){
     if(file.exists(output_file)){
        return(paste("File", output_file, "already exists."))
     }
  }
  rast <- gen_pop_gpw_c(input_file)
  saveRDS(rast, file = output_file)
}

make_diamonds <- function(input_file, output_file, overwrite){
   if(!overwrite){
      if(file.exists(output_file)){
         return(paste("File", output_file, "already exists."))
      }
   }
   #infile <- sf::st_read(input_file)
   rast <- gen_diamonds_s(input_file)
   saveRDS(rast, file = output_file)
}

make_growseas <- function(input_file, output_file, overwrite){
   if(!overwrite){
      if(file.exists(output_file)){
         return(paste("File", output_file, "already exists."))
      }
   }
   infile <- read.table(gzfile(input_file), header = T, sep = "\t")
   rast <- gen_growseas(infile)
   saveRDS(rast, file = output_file)
}

make_grow_agg <- function(input_file, output_file, overwrite){
   if(!overwrite){
      if(file.exists(output_file)){
         return(paste("File", output_file, "already exists."))
      }
   }
   infile <- read.table(gzfile(input_file), header = T, sep = "\t")
   rast <- gen_grow_start_end(infile)
   saveRDS(rast, file = output_file)
}

