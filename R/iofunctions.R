
stream.table <- function(filename, chunksize, ...){
   stream <- stream.file(filename, chunksize)
   header <- stream(1)

   function(...){
      lines <- stream()
      if(length(lines) > 0){
         tabletext <- c(header,lines)
      } else {
         tabletext <- header
      }
      print(tabletext)
      read.table(text = tabletext,
                 header = T,
                 ...)
   }
}

stream.file <- function(filename, chunksize, ...){
   infile <- file(filename)
   open(infile)

   lines <- 'init'
   function(n = NULL){
      if(length(lines) > 0){
         if(is.null(n)) n <- chunksize
         lines <<- readLines(infile, n = n)
         lines
      } else {
         # duck-close
         tryCatch(close(infile),
                  error = function(e){})
         character()
      }
   }
}

get_rds_file <- function(fname, output_folder){
   file_path <- paste0(output_folder, fname)
   robj <- readRDS(file_path)
   return(robj)
}

memoize <- function(fn,...,cache = "cache"){
   if(file.info(cache)$isdir){
      fnName <- as.character(substitute(fn))
      args <- sapply(as.list(...),as.character)
      repr <- paste0(c(fnName,args))
      cache <- file.path(cache,digest::sha1(repr))
   } else {

   }

   if(!file.exists(cache)){

      res <- fn(...)
      arrow::write_parquet(res,cache)
      res
   } else {
      arrow::read_parquet(cache)
   }
}
