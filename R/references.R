#' Get bibliography given a bibkey
#'
#' @param keys
#' @param bib.style
#' @param as_biblatex
#'
#' @return
#' @export
#'
#' @examples
get_bibliography <- function(keys = NULL, as_biblatex = FALSE){
  bib <- RefManageR::ReadBib("data/bibliography.bib")

  if(is.null(keys)){
    to_cite <- bib
  } else {
    to_cite <- bib[key = keys]
  }

  if(as_biblatex){
    return(RefManageR::toBiblatex(to_cite))
  }
  return(to_cite)
}

get_bib_element <- function(key, element = "author", as_character = TRUE){
  citation <- get_bibliography(key)
  if(element == "author"){
    res <- citation$author
  } else if(element == "journal"){
    res <- citation$journal
  } else if(element == "year"){
    res <- citation$year
  } else if(element == "title"){
    res <- citation$title
  } else{
    stop("Element not supported.")
  }

  if(as_character){
    return(res |> as.character())
  } else{
    return(res)
  }
}

extract_bib_elements <- function(citation_liststr, bib_element = "author", ...){
  citations <- stringr::str_split(citation_liststr, ",") |> trimws()
  element <- lapply(citations, get_bib_element, element = bib_element, ...)
  element
}
