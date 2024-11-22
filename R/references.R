get_bibliography <- function(keys, bib.style = "authoryear", as_biblatex = FALSE){
  bib <- RefManageR::ReadBib("data/pgmeta.bib")
  to_cite <- bib[key = keys]


  if(as_biblatex){
    return(RefManageR::toBiblatex(to_cite))
  } else {
    print(to_cite, .opts = list("bib.style" = bib.style))
  }
}


#' Add source to PRIO-GRID meta data
#'
#' @return
#' @export
#'
#' @examples
add_source <- function(){

}

#' Add variable to PRIO-GRID variable list
#'
#' @return
#' @export
#'
#' @examples
add_variable <- function(){

}

