
# ================================================

#' make_pg
#' 
#' A dispatch function that applies functions and assertions to data contained
#' in a folder hierarchy, with each folder under input_folder containing the
#' data needed to produce each variable. The function takes the input folder,
#' an outputfolder which is populated with the same folders as are contained in
#' the inputfolder, and an optional configfile, which is a YAML formatted
#' markupfile specifying what functions and assertions pertain to each
#' variable. The configuration file is specified per variable name, which
#' corresponds with the names of the folders contained in the inputfolder.
#' If no custom config file path is provided, the function tries to use the
#' config that is supplied with the package.
#' 
#' @param input_folder A folder containing folders with data for each variable 
#' @param output_folder The destination folder to which data will be written 
#' @param config An optional custom config file for custom PRIOGrid builds 
#' @param config An optional custom config file for custom PRIOGrid builds 
#'
#' @return The function returns a list of character vectors for each function
#' containing info about assertions.
#' 
#' @examples
#' var_messages <- make_pg("~/Projects/pg/pgdata","~/Projects/pg/outfolder"
#' purrr::walk(var_messages,function(queue){sapply(queue, warning)})
#' @export

make_pg <- function(input_folder, output_folder, config = NULL){

   if(is.null(config)){
      config <- paste(path.package("priogrid"),"config.yaml", sep = "/")
   }

   config <- yaml::yaml.load_file(config)

   messages <- lapply(list.files(inputfolder), function(f){
      dovar(f, outputfolder, conf[[f]])
   })

   messages 
}

# ================================================

#' dovar
#'
#' A dispatch function that applies the function specified in variable_config
#' to a variable_folder. Subsequently, the result is put through several
#' assertions, both a base set of assertions from base_assertions and possibly
#' also custom assertions specified in the variable_config. Then, data is
#' written to the output_folder in RDS format.
#' 
#' @param variable_folder A folder that is passed to the gen function
#' @param output_folder Where to write the data. The function creates a folder
#' with the same name as the variable folder.
#' @param variable_config A named list specifying fun and assert. Fun is the
#' name of the function to apply to the input_folder, while assert (optional)
#' is a character vector with assertions snippets to apply.
#' 
#' @return The function returns a character vector of messages with the results
#' of assertions. 
#' 
#' @examples
#' dovar("~/Projects/pg/pgdata/gwcodes",
#'       "~/Projects/pg/outfolder",
#'       pgBaseConfig(),
#' )
#' 

dovar <- function(variable_folder, output_folder, variable_config){
   results <- character() 
   rast <- eval(parse(text = variable_config$fun))(input_folder) 
   messages <- base_assertions(rast)

   if("assert" %in% names(variable_config) & length(variable_config$assert) > 0){
      messages <- c(messages , check_assertions(rast, variable_config$assert))
   }

   saveRDS(rast,paste(output_folder,input_folder,"data.rds", sep = "/"))
   messages 
}
# ================================================

#' check_assertions 
#'
#' Applies a custom battery of assertions to the provided raster layer, specified 
#' as a list of expressions in the config file. Each expression must evaluate to either  
#' true or false. The result is returned as a series of messages.
#' 
#' @param rast A raster layer.  
#' @param assertions A character vector of valid R expressions.
#' 
#' @return The function returns a character vector of messages with the results
#' of the assertions. 

check_assertions(rast,assertions){
   sapply(assertions, function(assertion){
      res <- eval(parse(text = assertion))
      if(res){
         glue("SUCCEEDED: {assertion}")
      } else {
         glue("FAILED: {assertion}")
      }
   })
}

# ================================================

#' base_assertions 
#'
#' Applies a basic battery of assertions to the provided grid, making sure it
#' has the correct size, projection, resolution, in relation to PRIOGrid.
#' 
#' @param rast A raster layer.  
#' 
#' @return The function returns a character vector of messages with the results
#' of the assertions. 

base_assertions(rast){
   assertions <- c(
      "raster::extent(rast) == prio_extent()",
      "nrow(rast) <- prio_nrow()",
      "ncol(rast) <- prio_ncol()",
      "raster::res(rast) == prio_resolution()",
      "raster::crs(rast) == prio_crs()"
   )
   check_assertions(rast,assertions)
}

