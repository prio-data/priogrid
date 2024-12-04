
#' read_geostat_CHG_out
#'
#' Reads the Geostat usual residence one year prior to cencus
#' moved from outside of reporting country
#'
#' @return an object of class raster
#' @export
read_geostat_CHG_out <- function() {
  f <- get_pgfile("GeostatCencusGrid", "2021")
  unzip_to <- file.path(dirname(f), tools::file_path_sans_ext(basename(f)))
  if (!dir.exists(unzip_to)) {
    dir.create(unzip_to, recursive = TRUE)
  }
  unzip(f, exdir = unzip_to)
  tiff_files <- list.files(unzip_to, pattern = "ESTAT_OBS-VALUE-CHG_OUT_2021_V2\\.tiff$", full.names = TRUE, recursive = TRUE)
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
  return(raster_object)
}

#' gen_CHG_out
#'
#' Takes the geostat CHG_out raster and returns a raster
#' in PRIO-GRID extent and resolution
#'
#' @export
CHG_out <- function() {
  gs <- read_geostat_CHG_out()
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


