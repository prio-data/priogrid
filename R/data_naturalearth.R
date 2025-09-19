#' Reads the Natural Earth 10m Physical Land Data
#'
#' Downloads and imports the Natural Earth 1:10m scale Physical Land dataset,
#' which provides global polygon boundaries of landmasses at a coarse
#' cartographic scale suitable for mapping and contextual analysis.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Downloads the Natural Earth 1:10m Physical Land shapefile from the
#'         PRIO-GRID data repository
#'   \item Extracts the contents of the zip archive
#'   \item Reads the shapefile into R as an \code{sf} object
#' }
#'
#' @return An \code{sf} object
#'
#' @note
#' \itemize{
#'   \item The Natural Earth dataset is intended for mapping and general
#'         analysis, not for precise cadastral or legal boundary use
#'   \item The file is cached locally after download
#' }
#'
#' @examples
#' \dontrun{
#' # Read Natural Earth 10m land polygons
#' land <- read_naturalearth_10m_land()
#'
#' # Inspect structure
#' print(land)
#'
#' # Plot global landmasses
#' plot(sf::st_geometry(land), col = "lightgray", border = "darkgray")
#' }
#'
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

#' Reads the Natural Earth Disputed Areas Dataset
#'
#' Downloads and imports the Natural Earth 1:10m scale Admin 0 Breakaway and
#' Disputed Areas dataset. This dataset depicts disputed and de facto political
#' boundaries, including areas with contested sovereignty or breakaway regions.
#'
#' @details
#' The dataset shows boundaries based on **control (de facto)** rather than
#' **international recognition (de jure)**. It is intended for visualization
#' and cartographic purposes. The dataset does not include temporal attributes,
#' so no specific time validity is provided.
#'
#' @return An \code{sf} object
#'
#' @note
#' \itemize{
#'   \item Boundaries represent political disputes as mapped by Natural Earth;
#'         they may not reflect all perspectives or claims
#'   \item Dataset is not dated and does not provide historical change tracking
#'   \item Intended for cartographic and analytical use, not for legal boundary
#'         definitions
#' }
#'
#' @examples
#' \dontrun{
#' # Read disputed areas polygons
#' disputed_areas <- read_ne_disputed_areas()
#'
#' # Inspect structure
#' print(disputed_areas)
#'
#' # Plot disputed areas
#' plot(sf::st_geometry(disputed_areas),
#'      col = "red", border = "darkred",
#'      main = "Natural Earth Disputed Areas")
#' }
#'
#' @export
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

#' Share of PRIO-GRID Cells Covered by Land (Natural Earth)
#'
#' Computes the proportion of each PRIO-GRID cell that intersects with land,
#' based on the Natural Earth 1:10m Physical Land dataset. Returns a raster
#' layer aligned to PRIO-GRID resolution with values in the range \code{[0, 1]}
#' indicating land coverage share.
#'
#' @return A single-layer \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Values of \code{0} correspond to ocean-only cells
#'   \item Values of \code{1} correspond to cells fully covered by land
#'   \item Intermediate values represent fractional coverage (e.g., coastal areas)
#' }
#'
#' @examples
#' \dontrun{
#' # Compute PRIO-GRID land cover share
#' land_share <- gen_naturalearth_cover_share()
#'
#' # Inspect values
#' summary(values(land_share))
#'
#' # Plot global land cover share
#' terra::plot(land_share,
#'             main = "Share of PRIO-GRID Cells Covered by Land")
#'
#' # Extract land share for a region (e.g., West Africa)
#' africa_extent <- terra::ext(-20, 20, 0, 20)
#' terra::plot(terra::crop(land_share, africa_extent),
#'             main = "Land Share in West Africa")
#' }
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

#' Land Mask for PRIO-GRID Cells (Natural Earth)
#'
#' Generates a binary raster mask indicating which PRIO-GRID cells intersect
#' with land, based on the Natural Earth land dataset. By default, any nonzero
#' land coverage qualifies a cell as land. The minimum proportion of land cover
#' required to classify a cell as land can be adjusted with \code{min_cover}.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Calls \code{\link{gen_naturalearth_cover_share}} to compute the
#'         fractional land cover of each grid cell
#'   \item Sets cells with land coverage below \code{min_cover} to \code{NA}
#'   \item Returns a raster aligned to PRIO-GRID resolution with non-\code{NA}
#'         values representing land cells
#' }
#'
#' @param min_cover Numeric, default \code{0}. Minimum fraction of a grid cell
#'   that must be covered by land for the cell to be classified as land.
#'   Should be between \code{0} and \code{1}.
#'
#' @return A single-layer \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Generate default land mask
#' land_mask <- gen_naturalearth_cover()
#' terra::plot(land_mask, main = "PRIO-GRID Land Mask (Natural Earth)")
#'
#' # Require 50% land cover to classify as land
#' land_mask50 <- gen_naturalearth_cover(min_cover = 0.5)
#' terra::plot(land_mask50, main = "PRIO-GRID Land Mask (≥50% Land)")
#' }
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

#' Generate Raster for Disputed Areas by Type
#'
#' Computes a PRIO-GRID raster representing the fraction of each cell covered
#' by disputed areas of a specified type, based on the Natural Earth Admin 0
#' Breakaway and Disputed Areas dataset.
#'
#' @param type Character string specifying the type of disputed area to include.
#'   Valid options are: "all", "breakaway", "disputed", "geo subunit", "geo unit",
#'   "indeterminate", "lease", or "overlay".
#' @param disputed_areas Optional \code{sf} object of disputed areas. Defaults
#'   to the result of \code{\link{read_ne_disputed_areas}}().
#'
#' @return A single-layer \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Generate raster for "disputed" areas
#' disputed_raster <- ne_disputed_area_share("disputed")
#' terra::plot(disputed_raster, main = "PRIO-GRID Coverage: Disputed Areas")
#'
#' # Generate raster combining all types
#' disputed_all <- ne_disputed_area_share("all")
#' terra::plot(disputed_all, main = "PRIO-GRID Coverage: All Disputed Areas")
#' }
#'
#' @export
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

#' Generate Raster for All Disputed Areas
#'
#' Creates a PRIO-GRID raster representing the fraction of each cell covered
#' by **all types** of disputed areas from the Natural Earth Admin 0 Breakaway
#' and Disputed Areas dataset. The dataset does not include temporal information,
#' so all disputes are treated as current/undated.
#'
#' @details
#' This function is a convenience wrapper around
#' \code{\link{ne_disputed_area_share}}, automatically using \code{type = "all"}
#' to combine all disputed area types into a single raster layer.
#'
#' @return A single-layer \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Generate raster covering all disputed areas
#' rast <- gen_ne_disputed_area_share()
#' terra::plot(rast, main = "PRIO-GRID Coverage: All Disputed Areas")
#' }
#'
#' @export
#' @references
#' \insertRef{naturalearthAdmin0Breakaway2024}{priogrid}
gen_ne_disputed_area_share <- function() {
  ne_disputed_area_share(type = "All")
}
