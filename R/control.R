
#' @title make_priogrid 
#' @description Make priogrid by following a specification that determines what functions are used for each variable, and where the data for each variable is located. The raw data are grouped under common names, since many variables use the same raw data.
#' @export
#' @importFrom magrittr %>%
make_priogrid <- function(input_folder,output_folder,
   specfile = NULL,format = "parquet"){

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

   # TODO Mockup
   spec <- spec[!is.na(spec$ready),]

   spec <- spec %>%
      apply(1,as.list) 


   for(varspec in spec){
      #call$fn <- eval(parse(text=paste0("priogrid::",varspec$genfunction)))
      procedure_call <- list(
         fn = varspec$genfunction,
         input_folder <- input_folder,
         output_folder <- output_folder,
         format <- format
      )

      do.call(mock_wrap_procedure,procedure_call)
   }
}

mock_wrap_procedure <- function(
   fn,
   input_folder,output_folder,
   format) {
   print(glue::glue("{deparse(fn)}: {input_folder}, {output_folder} to {format}"))
}

#' @title wrap_procedure
wrap_procedure<- function(fn,input_folder,output_folder,format = "parquet"){
   variable_function <- get_var_function(variable_name) 
   format_spec <- get_format_spec(format)

   infolder <- get_in_path(input_folder,variable_name)
   outpath <- get_out_path(output_folder,variable_name,format_spec$ext)

   # Check cache?
   if(file.exists(outpath)){
      data <- format_spec$read(outpath)
   } else {
      data <- variable_function(infolder)
      format_spec$write(data,outpath)
   }

   data
}

#' @title lookup 
lookup <- function(type,what){
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
variable_lookup <- function(...){lookup(type="variable",...)}

#' @title format_lookup 
format_lookup <- function(...){lookup(type="format",...)}

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

#' @title get_format_spec 
get_format_spec <- function(format_name){
   format_info <- format_lookup(format_name)
   format_info$fn <- eval(parse(text=var_info$fn))
   format_info
}

#' @title datapath 
#' @export

datapath <- function(folder){
   file.path(folder,"data")
}

