#' @export
#' @rdname PGOptionsManager
pgoptions <- NULL

.onLoad <- function(libname, pkgname) {
  assign("pgoptions", PGOptionsManager$new(), envir = parent.env(environment()))

  if(pgoptions$get_verbose()) pgoptions$print()
}
