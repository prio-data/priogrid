#' read_naturalearth_10m_physical
#'
#' Reads the Natural Earth 10m Physical Vector Data
#'
#' @return an object of class sf
#' @export
read_naturalearth_50m_land <- function(){
  f <- get_pgfile("NaturalEarth50mLand", "4.1.1")
  #unzip(f, list = T)
  unzip_to <- file.path(dirname(f), tools::file_path_sans_ext(basename(f)))
  unzip(f, exdir = unzip_to)
  return(sf::st_read(file.path(dirname(f), tools::file_path_sans_ext(basename(f)), "ne_50m_land.shp")))
}

#' gen_ne_coversh
#'
#' Takes the NaturalEarth 10m raster and returns a raster
#' in PRIO-GRID resolution with the share of the cell that intersects with land.
#'
#' @export
gen_ne_coversh <- function(){
  ne <- read_naturalearth_50m_land()

  pg <- prio_blank_grid()
  ne_coversh <- terra::rasterize(terra::vect(ne), pg, fun = "min", cover = T)
  names(ne_coversh) <- "ne_coversh"
  return(ne_coversh)
}

#' gen_ne_cover
#'
#' Takes the NaturalEarth 50m raster and returns
#' a raster-mask that is true for the grid cells that intersects with land.
#'
#' @export
gen_ne_cover <- function(){
  land_cover_share <- gen_ne_coversh()
  pg <- prio_blank_grid()
  ne_cover <- terra::intersect(land_cover_share, pg)
  names(ne_cover) <- "ne_cover"
  return(ne_cover)
}
