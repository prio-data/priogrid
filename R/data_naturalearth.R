read_naturalearth <- function(){
  f <- get_pgfile(src_name = "NaturalEarth10mPhysical", version = "4.1.1")
  #unzip(f, list = T)
  unzip_to <- file.path(dirname(f), tools::file_path_sans_ext(basename(f)))
  unzip(f, exdir = unzip_to)
  ne <- sf::st_read(file.path(dirname(f), tools::file_path_sans_ext(basename(f)), "ne_50m_land.shp"))
  return(ne)
}

#' gen_landcover_share
#'
#' Takes the NaturalEarth 50m raster and returns a raster
#' in PRIO-GRID resolution with the share of the cell that intersects with land.
#'
#' @export
gen_landcover_share <- function(){
  ne <- read_naturalearth()

  pg <- prio_blank_grid()
  land_cover <- rasterize(vect(ne), pg, fun = "min", cover = T)

  return(land_cover)
}

#' gen_pgland
#'
#' Takes the NaturalEarth 50m raster and returns
#' a raster-mask that is true for the grid cells that intersects with land.
#'
#' @export
gen_land <- function(){
  land_cover_share <- gen_landcover_share()
  pgland <- terra::intersect(land_cover, pg)

  return(pgland)
}
