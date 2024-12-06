#' Get the PRIO-GRID bibliography
#'
#' The bibliography contains all citations and further references that are
#' included in [pgsources]. You can also get a subset of references,
#' and you can get the [RefManageR::RefManageR-package] bibliography,
#' or references as biblatex.
#'
#' @param keys A vector of strings, bibkeys found in [pgsources].
#' @param as_biblatex Set to true if you want results in biblatex instead of an R object.
#'
#' @return BibEntry or BibTex
#' @export
#'
#' @examples
#' get_bibliography(keys = "schvitzMappingInternationalSystem2022")
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

#' Extract bibliography element
#'
#' Get bibliography information. Is used by [pgsearch()] to search [pgsources] based
#' on bibliography information.
#'
#' @param key A bibkey found in [pgsources]
#' @param element Supports author, journal, year, and title.
#' @param as_character Return the result as a string instead of a RefManageR class object.
#'
#' @return BibEntry-element class or vector with character strings
#' @export
#'
#' @examples
#' get_bib_element("schvitzMappingInternationalSystem2022", element = "author")
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

#' Helper function to parse bibliography elements based on a source
#'
#' In [pgsources], citations are semi-colon separated. This function splits
#' these into individual keys and get the bibliography element from each key.
#'
#' @param citation_liststr A semi-colon separated list of bibliography keys from [pgsources]
#' @param bib_element Supports author, journal, year, and title.
#' @param ...
#'
#' @return list with BibEntry-element classes or vectors with character strings
#' @export
#'
#' @examples
#' extract_bib_elements(pgsources$citation_keys[1])
extract_bib_elements <- function(citation_liststr, bib_element = "author", ...){
  citations <- stringr::str_split(citation_liststr, ";") |> trimws()
  element <- lapply(citations, get_bib_element, element = bib_element, ...)
  element
}
