
#' @title make_priogrid 
make_priogrid <- function(){
    
}

#' @title wrap_procedure
wrap_procedure<- function(variable_name,
   input_folder,output_folder,format = "parquet"){

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

