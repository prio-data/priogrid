#' Read Natural Earth Disputed Areas dataset
#'
#' Reads the Admin 0 – Breakaway, Disputed Areas dataset from Natural Earth.
#' This dataset shows de facto boundaries based on control versus de jure.
#' The data entries are not dated.
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' # Load disputed areas dataset
#' disputed_areas <- read_disputed_areas()
#' plot(sf::st_geometry(disputed_areas), main = "Disputed Areas")
#' @references
#' \insertRef{naturalearthAdmin0Breakaway2024}{priogrid}
read_ne_disputed_areas <- function() {
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

#' Generate raster for disputed areas by type of dispute
#'
#' Creates a raster in PRIO-GRID resolution for a specified type of disputed area.
#' The options include: "all", "breakaway", "disputed", "geo subunit","geo unit", "indeterminate", "lease" and "overlay"
#'
#' @param type Character string indicating the disputed area type (e.g., "Disputed").
#' @param disputed_areas sf object of disputed areas, default is from `read_disputed_areas()`.
#' @return terra SpatRaster
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate raster for "Disputed" type
#'   disputed_raster <- ne_disputed_area_share("Disputed")
#'   terra::plot(disputed_raster, main = "Disputed Areas: Disputed")
#' }
#'
#' @references
#' \insertRef{naturalearthAdmin0Breakaway2024}{priogrid}
ne_disputed_area_share <- function(type) {
  if(!is.character(type)){
    stop("Invalid type. Please choose from: ", paste(valid_types, collapse = ", "))
  }

  disputed_areas = read_ne_disputed_areas()

  # Consistent lower-case
  disputed_areas$TYPE <- tolower(disputed_areas$TYPE)
  type <- tolower(type)

  valid_types <- c("all", unique(disputed_areas$TYPE))
  if (!type %in% valid_types) {
    stop("Invalid type. Please choose from: ", paste(valid_types, collapse = ", "))
  }

  if(type == "all"){
    df <- disputed_areas
  } else{
    df <- disputed_areas |> dplyr::filter(TYPE == type)
  }

  pgcrs <- sf::st_crs(pgoptions$get_crs())
  if (sf::st_crs(df) != pgcrs) {
    df <- sf::st_transform(df, crs = pgcrs)
  }

  pg <- prio_blank_grid()

  data_combined <- df |> dplyr::summarize(geometry = sf::st_combine(geometry))
  coversh <- exactextractr::exact_extract(pg, data_combined)

  ra <- exactextractr::rasterize_polygons(data_combined, pg)
  pg <- pg*ra # Remove non-land cells

  r <- terra::classify(pg, coversh[[1]])
  names(r) <- paste0("ne_disputed_area_share", type)

  return(r)
}

#' Generate raster for disputed areas by type of dispute
#'
#' Creates a raster in PRIO-GRID resolution for disputed areas from the
#' Natural Earth Disputed Areas dataset using all types of disputes. The timing
#' of the disputes are not dated.
#'
#' @return terra SpatRaster
#' @export
#'
#' @examples
#' \dontrun{
#'   rast <- gen_ne_disputed_area_share()
#' }
#' @references
#' \insertRef{naturalearthAdmin0Breakaway2024}{priogrid}
gen_ne_disputed_area_share <- function() {
  ne_disputed_area_share(type = "All")
}
