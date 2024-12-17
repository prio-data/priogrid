#' Reads the Natural Earth 10m Physical Vector Data
#'
#' @return an object of class sf
#' @export
#' @references
#' \insertRef{naturalearthLand10mPhysical2024}{priogrid}
read_naturalearth_10m_land <- function(){
  f <- get_pgfile(source_name = "Natural Earth Physical 10m Land",
                  source_version = "5.1.1",
                  id = "92da9800-4520-4e87-a855-b28255452189")
  #unzip(f, list = T)
  unzip_to <- file.path(dirname(f), tools::file_path_sans_ext(basename(f)))
  unzip(f, exdir = unzip_to)
  return(sf::st_read(file.path(dirname(f), tools::file_path_sans_ext(basename(f)), "ne_10m_land.shp")))
}

#' Share of grid-cell that intersects with land (NaturalEarth)
#'
#' Takes the NaturalEarth 10m raster and returns a raster
#' in PRIO-GRID resolution with the share of the cell that intersects with land.
#'
#' @export
#' @references
#' \insertRef{naturalearthLand10mPhysical2024}{priogrid}
gen_naturalearth_cover_share <- function(){
  ne <- read_naturalearth_10m_land()
  pg <- prio_blank_grid()

  ne_combined <- ne |> dplyr::summarize(geometry = sf::st_combine(geometry))
  coversh <- exactextractr::exact_extract(pg, ne_combined)

  ra <- exactextractr::rasterize_polygons(ne_combined, pg)
  pg <- pg*ra # Remove non-land cells

  res <- terra::classify(pg, coversh[[1]])

  names(res) <- "naturalearth_cover_share"
  return(res)
}

#' Whether or not a grid-cell intersects with land (NaturalEarth)
#'
#' Takes the NaturalEarth 50m raster and returns
#' a raster-mask that is true for the grid cells that intersects with land.
#'
#' @export
#' @references
#' \insertRef{naturalearthLand10mPhysical2024}{priogrid}
gen_naturalearth_cover <- function(min_cover = 0){
  land_cover_share <- gen_naturalearth_cover_share()

  land_cover_share <- terra::ifel(land_cover_share < min_cover, NA, land_cover_share)
  pg <- prio_blank_grid()
  ne_cover <- terra::intersect(land_cover_share, pg)
  names(ne_cover) <- "naturalearth_cover"
  return(ne_cover)
}
