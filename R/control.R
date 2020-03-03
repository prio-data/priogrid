
#' @title wrapProcedure
#' @description 
#' @param
#' @param
makeVariable <- function(variableName,inputFolder,outputFolder,format = "parquet"){

   variableFunction <- getVarFunction(variableName) 
   formatSpec <- getFormatSpec(format)

   infolder <- getInPath(inputFolder,variableName)
   outpath <- getOutPath(outputFolder,variableName,formatSpec$ext)

   # Check cache?
   if(file.exists(outpath)){
      data <- formatSpec$read(outpath)
   } else {
      data <- variableFunction(infolder)
      formatSpec$write(data,outpath)
   }

   data
}

#' @title lookup 
#' @description
#' @param
#' @param
lookup <- function(type,what){
   if(!type %in% c("variable","format")){
      stop("Lookup type must be one of [variable, format]")
   }
   
   fname <- switch(type,
      variable = "variableSpecification.yaml",
      format = "formatSpecification.yaml"
   )
   fname <- system.file(
      "extdata",
      fname,
      package = "priogrid",
      mustWork = TRUE
   )
   tryCatch({lookupList <- yaml::yaml.load_file(fname)},
      error = function(e){
         stop("Spec file not found. Was the package installed correctly?")
      })
   lookupEntry <- lookupList[[what]]
   if(is.null(lookupEntry)){
      stop(glue::glue("{what} has no {type} specification in {fname}"))
   }

   lookupEntry
}

#' @title variableLookup 
#' @description
#' @param
#' @param
variableLookup <- function(...){lookup(type="variable",...)}

#' @title formatLookup 
#' @description
#' @param
#' @param
formatLookup <- function(...){lookup(type="format",...)}

#' @title getInPath 
#' @description
#' @param
#' @param
getInPath <- function(folder,name){
   file.path(folder,name,"data")
}

#' @title getOutPath 
#' @description
#' @param
#' @param
getOutPath <- function(folder,name,ext){
   if(!grepl("^\\.",ext)){
      ext <- paste0(".",ext)
   }

   file.path(folder,paste0(name,ext))
}

#' @title getVarFunction 
#' @description
#' @param
#' @param
getVarFunction <- function(variableName){
   varInfo <- variableLookup(variableName)
   functionName <- paste0(varInfo$fn)
   eval(parse(text=functionName))
}

#' @title getFormatSpec 
#' @description
#' @param
#' @param
getFormatSpec <- function(formatName){
   formatInfo <- formatLookup(formatName)
   formatInfo$fn <- eval(parse(text=varInfo$fn))
   formatInfo
}
