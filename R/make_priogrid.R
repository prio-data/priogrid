
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

make_pg <- function(input_folder, output_folder, config = NULL, overwrite = FALSE){
   if(is.null(config)){
      config <- priogrid::prio_config()
   }

   writeLines(priogrid::makeheader)

   apply(config,1,function(variable_config){
      variable_config <- as.list(variable_config)
      path <- file.path(input_folder,variable_config$path)

      dest <- file.path(output_folder,variable_config$name)
      if(file.exists(dest) &! overwrite){
         variable_config$fun <- "function(x){stop(\"Output file exists\")}"
      }
      writeColored(glue::glue("{strrep('*',64)}\nINFO: Doing {variable_config$name}"))

      m1 <- Sys.time()

      msgs <- tryCatch(dovar(path, output_folder, variable_config),
               error = function(e){paste(variable_config$name," | ERROR:",e$message)})

      m2 <- Sys.time()

      msgs <- c(msgs,glue::glue(
         "{variable_config$name} | INFO: {format(m2-m1)} elapsed."
      ))

      sapply(msgs, writeColored)


      msgs
   })
}

# ================================================

#' prio_config 
#' 
#' A helper function that returns the standard configuration for
#' making priogrid.
#' 
#' Useful for substituting the standard functions with new ones,
#' or only doing certain variables.
#' 
#' @return Returns a list of named lists containing configuration
#' info.
#' 
#' @export

prio_config <- function(){
   file.path(find.package("priogrid"),"conf.yaml") %>%
      yaml::yaml.load_file() %>%
      dplyr::bind_rows()
}

# ================================================

#' dovar
#'
#' A dispatch function that applies the function specified in variable_config
#' to the file in path. Subsequently, the result is put through several
#' assertions, both a base set of assertions from base_assertions and possibly
#' also custom assertions specified in the variable_config. Then, data is
#' written to the output_folder in RDS format.
#' 
#' @param path A file path that is passed to the gen function 
#' @param Where to write the data. The function creates a folder
#' with the same name as the variable folder.
#' @param variable_config A named list specifying fun and assert. Fun is the
#' name of the function to apply to the input_folder, while assert (optional)
#' is a character vector with assertions snippets to apply.
#' 
#' @return The function returns a character vector of messages with the results
#' of assertions. The data is written to disk.

dovar <- function(path, output_folder, variable_config){

   # ~ This is where the magic happens ~ 
   rast <- eval(parse(text = variable_config$fun))(path) 

   messages <- base_assertions(rast)

   # ! Force inmemory !
   if(!raster::inMemory(rast)){
      rast <- raster::readAll(rast)
   }
   saveRDS(rast,file.path(output_folder,variable_config$name))
   paste(variable_config$name,messages, sep = ' | ')
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

check_assertions <- function(rast,assertions){
   sapply(assertions, function(assertion){
      res <- eval(parse(text = assertion))
      if(res){
         paste0("SUCCEEDED: ",assertion)
      } else {
         msg <- paste0("FAILED: ",assertion)
         if(stringr::str_detect(assertion,"\\=\\=")){
            exp <- stringr::str_split(assertion, "==")[[1]]
            vals <- sapply(exp,function(e){eval(parse(text = e))})
            msg <- paste(msg,crayon::red(glue::glue("{vals[1]} {crayon::yellow('!=')} {vals[2]}")))
         }
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

base_assertions <- function(rast){
   assertions <- c(
      "raster::extent(rast) == prio_extent()",
      "nrow(rast) == prio_nrow()",
      "ncol(rast) == prio_ncol()",
      "max(raster::res(rast)) == prio_resolution()",
      "as.character(raster::crs(rast)) == prio_crs()"
   )
   check_assertions(rast,assertions)
}

# ================================================

#' writeColored
#'
#' writeLines wrapper that colors certain things in the output
#'
#' @param x A string
#'
#'
#' @examples
#' writeColored("ERROR: this will be red!")
#'

writeColored <- function(x){
   wrappings <- list(
      "ERROR:" = crayon::red,
      "SUCCEEDED:" = crayon::green,
      "FAILED:" = crayon::yellow,
      "INFO:" = crayon::blue
   )
   for(i in 1:length(wrappings)){
      color <- names(wrappings)[i]
      fun <- wrappings[[i]]
      x <- gsub(color,fun(color),x)
   }
   writeLines(x)
}
