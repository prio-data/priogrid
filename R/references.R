get_bibliography <- function(keys, bib.style = "authoryear", as_biblatex = FALSE){
  bib <- RefManageR::ReadBib("data/pgmeta.bib")
  to_cite <- bib[key = keys]


  if(as_biblatex){
    return(RefManageR::toBiblatex(to_cite))
  } else {
    print(to_cite, .opts = list("bib.style" = bib.style))
  }
}
