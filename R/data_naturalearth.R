#' read_naturalearth_10m_physical
#'
#' Reads the Natural Earth 10m Physical Vector Data
#'
#' @return an object of class sf
#' @export
read_naturalearth_10m_physical <- function(){
  f <- get_pgfile("NaturalEarth10mPhysical", "4.1.1")
  #unzip(f, list = T)
  unzip_to <- file.path(dirname(f), tools::file_path_sans_ext(basename(f)))
  unzip(f, exdir = unzip_to)
  return(sf::st_read(file.path(dirname(f), tools::file_path_sans_ext(basename(f)), "ne_50m_land.shp")))
}

#' gen_landcover_share
#'
#' Takes the NaturalEarth 10m raster and returns a raster
#' in PRIO-GRID resolution with the share of the cell that intersects with land.
#'
#' @export
gen_landcover_share <- function(){
  ne <- read_naturalearth_10m_physical()

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
  pg <- prio_blank_grid()
  pgland <- terra::intersect(land_cover_share, pg)

  return(pgland)
}
