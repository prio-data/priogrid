
#' Reads the Geostat usual residence total population
#'
#' @return SpatRaster
#' @export
#'
#' @references
#' \insertRef{geographicinformationsystemofthecommissionGeostatCensusGrid2024}{priogrid}
read_geostat_pop <- function() {
  f <- get_pgfile(source_name = "GISCO Geostat Census Grid",
                  source_version = "2021",
                  id = "86532b44-ce5c-48a6-96f7-704885a9afb2")
  unzip_to <- file.path(dirname(f), tools::file_path_sans_ext(basename(f)))
  if (!dir.exists(unzip_to)) {
    dir.create(unzip_to, recursive = TRUE)
  }
  unzip(f, exdir = unzip_to)
  tiff_files <- list.files(unzip_to, pattern = "ESTAT_OBS-VALUE-T_2021_V2\\.tiff$", full.names = TRUE, recursive = TRUE)
  if (length(tiff_files) == 0) {
    stop("No TIFF files found in the unzip directory")
  }
  if (length(tiff_files) > 1) {
    warning("Multiple TIFF files found, using the first one")
  }
  tiff_file <- tiff_files[1]
  if (!file.exists(tiff_file)) {
    stop("File does not exist: ", tiff_file)
  }
  raster_object <- terra::rast(tiff_file)

  tif_date <- lubridate::ymd("2024-06-16") |> as.character()

  names(raster_object) <- tif_date
  return(raster_object)
}

#' gen_pop_residence
#'
#' Takes the geostat totpop raster and returns a spatraster
#' in PRIO-GRID extent and resolution
#'
#' @return SpatRaster
#' @export
#'
#'@examples
#' # gs <- gen_geostat_pop_residence
#'
#' @references
#' \insertRef{geographicinformationsystemofthecommissionGeostatCensusGrid2024}{priogrid}
gen_geostat_pop_residence <- function() {
  gs <- read_geostat_pop()
  pg <- prio_blank_grid()
  gs <- terra::project(gs, terra::crs(pg))
  higher_resolution <- terra::res(gs) < terra::res(pg)
  if(any(higher_resolution)){
    gsagg <- terra::aggregate(gs,
                           fact = terra::res(pg)/terra::res(gs),
                           fun = "median")
  }
  equal_extent <- terra::ext(gsagg) == terra::ext(pg)
  if(!equal_extent){
    tmp <- terra::rast(terra::ext(pg),
                       crs = terra::crs(gsagg),
                       ncol = ncol(pg),
                       nrow = nrow(pg))
    gs_resamp <- terra::resample(gsagg, tmp, method = "near", threads = T)
  }
  names(gs_resamp) <- "geostat_pop_residence"
  return(gs_resamp)
}



