
rollovercols <- function(data,winsize = 3){
   if(winsize %% 2 == 0){
      stop(glue::glue('Window size must be odd, not {winsize}!'))
   } 
   dnames <- names(data)
   months <- dnames[!dnames %in% c('lat','long')]

   winmid <- ceiling(winsize / 2)
   winmargin <- (winsize - 1) / 2
   mids <- seq(winmid,length(months) - winmid,1)

   rowindices <- 1:length(months) 
   winindices <- c(tail(rowindices,winmargin),
                      rowindices,
                      head(rowindices,winmargin))
   print(glue::glue('windices: {glue::glue_collapse(winindices,sep = \',\')}'))

   apply(data,1,function(row){
      row <- row[months]
      
      sapply(1:length(row),function(colindex){
         mnames <- names(row)
         wincenter  <- colindex + winmargin
         w <- seq(colindex,(winsize + colindex) - 1)
         window <- winindices[w]
         print(glue::glue('Values: {glue::glue_collapse(row[window],sep = \',\')}'))
         print(glue::glue('Names: {glue::glue_collapse(mnames[window],sep = \',\')}'))

         print(glue::glue('Index: {colindex}'))
         print(glue::glue('Center: {wincenter}'))
         print(glue::glue('Sum: {sum(row[window])}'))
      })
   })
}
