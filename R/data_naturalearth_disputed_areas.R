#' Read Natural Earth Disputed Areas dataset
#'
#' Reads the Admin 0 – Breakaway, Disputed Areas dataset from Natural Earth.
#' This dataset shows de facto boundaries based on control versus de jure.
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' # Load disputed areas dataset
#' disputed_areas <- read_disputed_areas()
#' plot(sf::st_geometry(disputed_areas), main = "Disputed Areas")
#' @references
#' \insertRef{naturalearthLand10mPhysical2024}{priogrid}
read_disputed_areas <- function() {
  f <- get_pgfile(
    source_name = "Natural Earth Breakaway and Disputed Areas",
    source_version = "5.1.1",
    id = "920663ad-d7e7-4528-b36d-4b7266def2b1"
  )
  fname <- file.path(dirname(f), "ne_10m_admin_0_disputed_areas.shp")
  if (!file.exists(fname)) {
    unzip(f, exdir = dirname(f))
  }
  return(sf::read_sf(fname))
}

#' Generate raster for disputed areas coverage
#'
#' Creates a raster in PRIO-GRID resolution indicating the share of each cell
#' covered by disputed areas from the Natural Earth dataset.
#'
#' @param data sf object of disputed areas, default is from `read_disputed_areas()`.
#' @return terra SpatRaster
#' @export
#'
#' @examples
#' # Generate disputed areas coverage raster
#' disputed_cover <- gen_disputed_areas_cover()
#' terra::plot(disputed_cover, main = "Disputed Areas Coverage")
#' @references
#' \insertRef{naturalearthLand10mPhysical2024}{priogrid}
gen_disputed_areas_cover <- function(data = read_disputed_areas()) {
  if (sf::st_crs(data) != sf::st_crs(4326)) {
    data <- sf::st_transform(data, crs = 4326)
  }
  pg <- prio_blank_grid()
  disputed_coversh <- terra::rasterize(
    terra::vect(data), pg,
    fun = function(vals, ...) mean(vals),
    cover = TRUE
  )
  names(disputed_coversh) <- "disputed_land_share_of_cell"
  return(disputed_coversh)
}

#' Generate raster for disputed areas by type of dispute
#'
#' Creates a raster in PRIO-GRID resolution for a specified type of disputed area.
#' The options include: "Breakaway", "Disputed", "Geo subunit","Geo unit", "Indeterminate", "Lease" and "Overlay"
#' If no type is specified, defaults to "Disputed".
#'
#' @param type Character string indicating the disputed area type (e.g., "Disputed").
#' Default is "Disputed".
#' @param disputed_areas sf object of disputed areas, default is from `read_disputed_areas()`.
#' @return terra SpatRaster
#' @export
#'
#' @examples
#' # Generate raster for "Disputed" type (default)
#' disputed_raster <- gen_naturalearth_disputed_areas_type()
#' terra::plot(disputed_raster, main = "Disputed Areas: Disputed")
#'
#' # Generate raster for another type (e.g., "Breakaway")
#' breakaway_raster <- gen_naturalearth_disputed_areas_type(type = "Breakaway")
#' terra::plot(breakaway_raster, main = "Disputed Areas: Breakaway")
#' @references
#' \insertRef{naturalearthLand10mPhysical2024}{priogrid}
gen_naturalearth_disputed_areas_type <- function(type = "Disputed", disputed_areas = read_disputed_areas()) {
  if (!type %in% unique(disputed_areas$TYPE)) {
    stop("Invalid type. Please choose from: ", paste(unique(disputed_areas$TYPE), collapse = ", "))
  }

  disputed_type <- disputed_areas |>
    dplyr::filter(TYPE == type) |>
    terra::vect()

  pg <- prio_blank_grid()

  r <- terra::rasterize(disputed_type, pg, fun = "max", cover = TRUE)

  names(r) <- paste0("naturalearth_disputed_areas_", type) |> tolower()

  return(r)
}

