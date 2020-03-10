
#' @title make_priogrid 
#'
#' @description Make priogrid.
#' This function is the main entrypoint for the priogrid package. It compiles
#' the files that make up priogrid by following several different specification
#' files located under extdata (or specified).  
#'
#' This function is responsible for figuring out which function to run for each
#' variable, and providing these functions with the location of the raw data,
#' as well as the location to which the resulting data will be written, and in
#' what file format.
#' 
#' @param input_folder The folder containing the raw data file structure (see
#' vignette('pg_raw_data')
#' @param output_folder The folder where the finished data will be written.
#' @param specfile A specification file which tells the function what to make.
#' THis file must follow a format laid out in vignette('pg_specification') 
#' @param format What file format to write out. Currently supported formats are
#' 'parquet' (default) and 'rds'. 
#' @export 
#' @importFrom magrittr %>%

make_priogrid <- function(input_folder,output_folder,
   specfile = NULL,format = "parquet"){

   # TODO
   # 
   # This function should also be responsible for running checks on the input
   # and output, to determine if the files that are read / written are correct.
   # 
   # Much of this functionality is already implemented in the integrity.R file.
   # 

   # TODO Mockup
   input_folder <- "/tmp/in"
   output_folder <- "/tmp/out"
   specfile <- "/tmp/spec.csv"

   # Read the included specfile by default
   if(is.null(specfile)){
      specfile <- system.file(
         "extdata",
         "authorative_variable_names.csv",
         package = "priogrid",
         mustWork = TRUE
      )
   }
   spec <- read.csv(specfile,stringsAsFactors = FALSE)

   # TODO Mockup. Remove this!
   spec <- spec[!is.na(spec$ready),]

   # Convert dataset to row-lists
   spec <- spec %>%
      apply(1,as.list) 


   # For line in spec, run the function with the provided arguments.
   for(varspec in spec){
      procedure_call <- list(
         fn = parse_gen_function(varspec$genfunction),
         input_folder <- input_folder,
         output_folder <- output_folder,
         format <- format
      )

      do.call(mock_wrap_procedure,procedure_call)
   }
}

#' @title wrap_procedure
#'
#' @description This function manages a procedure, which is defined as the
#' application of a (gen) function to raw data, yielding a data frame. This
#' data frame is then saved in the given format.
#' 
#' Each gen function must manage its own raw data path. The wrap procedure only
#' provides the folder containing the file structure that hosts the raw data
#' folders.
#'
#' The wrapper also avoids doing any work if the output file exists. If the
#' output file should be remade, delete it before running the make function. 
#'
#' This function should not be called directly, but is rather called by the
#' make function. If you want to build a single variable, call the gen function
#' directly instead.
#'
#' @param fn The function to apply
#' @param input_folder The folder containing the priogrid raw data. 
#' @param output_folder Where to write the output data files 
#' @param format What format to write the output data to 
wrap_procedure <- function(fn, input_folder,output_folder, format) {

   # This returns a list that specifies r/w functions and the file ext.
   format_spec <- get_format_spec(format)

   out_path <- get_out_path(output_folder,variable_name,format_spec$ext)
   if(file.exists(out_path)){
      data <- format_spec$read(out_path)
   } else {
      data <- fn(input_folder)
      format_spec$write(data,outpath)
   }
}

#' @title parse_gen_function
#' @description Checks if a function is exported from priogrid and returns it. 
parse_gen_function <- function(function_name){
   # TODO check if function is actually exported from priogrid::
   eval(parse(text=paste0("priogrid::",function_name)))
}


#' @title get_in_path 
get_in_path <- function(folder,name){
   file.path(folder,name,"data")
}

#' @title get_out_path 
get_out_path <- function(folder,name,ext){
   if(!grepl("^\\.",ext)){
      ext <- paste0(".",ext)
   }

   file.path(folder,paste0(name,ext))
}

#' @title get_var_function 
get_var_function <- function(variable_name){
   var_info <- variable_lookup(variable_name)
   function_name <- paste0(var_info$fn)
   eval(parse(text=function_name))
}


#' @title lookup 
#' @description Returns an entry from a specification list, which is located in
#' the package's extdata folder. This function is used to retrieve data about
#' how to read/write file extensions (mm).
specification_lookup <- function(type,what){
   if(!type %in% c("variable","format")){
      stop("Lookup type must be one of [variable, format]")
   }
   
   fname <- switch(type,
      variable = "variable_specification.yaml",
      format = "format_specification.yaml"
   )
   fname <- system.file(
      "extdata",
      fname,
      package = "priogrid",
      mustWork = TRUE
   )
   tryCatch({lookup_list <- yaml::yaml.load_file(fname)},
      error = function(e){
         stop("Spec file not found. Was the package installed correctly?")
      })
   lookup_entry <- lookup_list[[what]]
   if(is.null(lookup_entry)){
      stop(glue::glue("{what} has no {type} specification in {fname}"))
   }

   lookup_entry
}

#' @title variable_lookup 
#'
variable_lookup <- function(...){specification_lookup(type="variable",...)}

#' @title format_lookup 
#'
format_lookup <- function(...){specification_lookup(type="format",...)}

#' @title get_format_spec 
#'
#' @description Returns the spec for a format, with functions for reading and
#' writing as well as the file extension to use.
#' Used for dispatching gen functions with different file formats.
#' See extdata/format_specification.yaml for further information.
get_format_spec <- function(format_name){
   format_info <- format_lookup(format_name)
   format_info$read <- eval(parse(text=var_info$read))
   format_info$write <- eval(parse(text=var_info$write))
   format_info
}
