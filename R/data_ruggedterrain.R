#' Read Global Multi-resolution Terrain Elevation Data (GMTED2010)
#'
#' Reads the GMTED2010 spatial metadata for terrain elevation as an \code{sf} object.
#' This metadata includes polygon geometries for GMTED2010 tiles and summary
#' elevation statistics such as minimum, maximum, and mean elevation values.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Locates the local GMTED2010 dataset file using \code{\link{get_pgfile}}
#'   \item Unzips the file to a local directory
#'   \item Reads the spatial metadata shapefile from the extracted data
#' }
#'
#' @return An \code{sf} object
#'
#' @examples
#' \dontrun{
#' # Read the GMTED2010 elevation data
#' elevation_data <- read_ruggedterrain()
#' print(elevation_data)
#' }
#'
#' @seealso
#' \code{\link{ruggedterrain_variable}} for calculating elevation statistics,
#' \code{\link{get_pgfile}} for the underlying data retrieval function
#'
#' @export
#' @references
#' \insertRef{danielsonGlobalMultiresolutionTerrain2011}{priogrid}
read_ruggedterrain <- function() {
  # Locate the GMTED2010 zip file
  zip_file <- get_pgfile(
    source_name = "Global Multi-resolution Terrain Elevation Data",
    source_version = "GMTED2010",
    id = "8c8192eb-cc29-4598-8f8a-ec190ba35c2d"
  )

  # Create extraction directory (same location as zip, without .zip extension)
  zip_basename <- tools::file_path_sans_ext(basename(zip_file))
  extract_dir <- file.path(dirname(zip_file), zip_basename)

  # Build path to the shapefile
  shapefile_path <- file.path(
    extract_dir,
    "GMTED2010_Spatial_Metadata",
    "GMTED2010_Spatial_Metadata.shp"
  )

  # Extract the zip file only if shapefile doesn't already exist
  if (!file.exists(shapefile_path)) {
    unzip(zip_file, exdir = extract_dir)
  }

  # Read and return the shapefile
  sf::st_read(shapefile_path)
}

#' Calculate elevation in PRIO-GRID format with set variable
#'
#' Estimates either the min, max, or mean of elevation in each PRIO-GRID cell
#' based on vector data from GMTED2010.
#'
#' @param variable Character string indicating elevation function.
#' Must be one of: "elevation_min", "elevation_max", "elevation_mean"
#'
#' @return A \code{SpatRaster} object
#' @export
#'
#' @examples
#' \dontrun{
#' # Get mean elevation for each PRIO-GRID cell
#' mean_elev <- ruggedterrain_variable(variable = "elevation_mean")
#'
#' # Get minimum elevation for each PRIO-GRID cell
#' min_elev <- ruggedterrain_variable(variable = "elevation_min")
#'
#' # Get maximum elevation for each PRIO-GRID cell
#' max_elev <- ruggedterrain_variable(variable = "elevation_max")
#' }
#'
#' @references
#' \insertRef{danielsonGlobalMultiresolutionTerrain2011}{priogrid}
ruggedterrain_variable <- function(variable) {
  rt <- read_ruggedterrain()
  pg <- prio_blank_grid()

  if (!variable %in% c("elevation_min", "elevation_max", "elevation_mean")) {
    stop("Invalid variable. Choose from 'elevation_min', 'elevation_max', 'elevation_mean'.")
  }

  # If source data is different than PRIO-GRID projection, transform it to desired projection.
  if(sf::st_crs(rt) != sf::st_crs(pg)){
    rt <- sf::st_transform(rt, sf::st_crs(pg))
  }

  col_mapping <- c("elevation_min" = "MIN_ELEV",
                   "elevation_max" = "MAX_ELEV",
                   "elevation_mean" = "MEAN_ELEV")

  # Get the original column name
  source_col <- col_mapping[variable]

  # Check if the required column exists
  if (!source_col %in% names(rt)) {
    stop(paste("Column", source_col, "not found in elevation data"))
  }

  coversh <- exactextractr::exact_extract(pg, rt)
  names(coversh) <- rt[[source_col]]
  matching_table <- dplyr::bind_rows(coversh, .id = variable)

  # Function mapping
  fun_map <- list(
    mean = function(x, w) weighted.mean(x, w, na.rm = TRUE),
    min = function(x, w) min(x, na.rm = TRUE),
    max = function(x, w) max(x, na.rm = TRUE)
  )

  myfun <- sub("elevation_", "", variable)

  matching_table <- matching_table |>
    dplyr::group_by(value) |>
    dplyr::mutate(!!variable := as.numeric(!!rlang::sym(variable))) |>
    dplyr::summarize(!!variable := fun_map[[myfun]](!!rlang::sym(variable), coverage_fraction))

  values(pg)[!values(pg) %in% matching_table$value] <- NA
  res <- terra::classify(pg, matching_table)

  return(res)
}

#' Generate the average (mean) elevation for PRIO-GRID cells
#'
#' A convenience wrapper function that calculates mean elevation values for each
#' PRIO-GRID cell using \code{\link{ruggedterrain_variable}}.
#'
#' @return A \code{SpatRaster} object
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate mean elevation raster
#' mean_elevation <- gen_ruggedterrain_elevation_mean()
#' }
#'
#' @seealso \code{\link{ruggedterrain_variable}} for the underlying function that
#' supports min, max, and mean elevation calculations
#'
#' @references
#' \insertRef{danielsonGlobalMultiresolutionTerrain2011}{priogrid}
gen_ruggedterrain_elevation_mean <- function(){
  r <- ruggedterrain_variable("elevation_mean")
  names(r) <- "elevation_mean"
  r
}

