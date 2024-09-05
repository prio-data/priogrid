
#' Set package-options for PRIO-GRID
#' @param opt_list A named list with the option name and option value. Parameters:
#'  ncol = number of columns in prio-grid output raster
#'  nrow = number of rows in prio-grid output raster
#'  ext = extent of prio-grid output raster
#'  crs = projections of prio-grid output raster
#'  rawfolder = folder to download and store rawdata
#' @examples
#' #set_pg_options(list("rawfolder" = "~/priogrid_data/"))
#' @export
set_pg_options <- function(opt_list){
  pgopts <- options()$PG.options
  if(is.null(pgopts)){pgopts <- list()}
  pgopts <- utils::modifyList(pgopts, opt_list)
  options(PG.options = pgopts)
}

if(is.null(options()$PG.options)){
  set_pg_options(list(
    ncol = 720,
    nrow = 360,
    ext = c("xmin"=-180, "xmax"=180, "ymin"=-90, "ymax"=90),
    crs = "epsg:4326",
    rawfolder = ""
  ))
}

