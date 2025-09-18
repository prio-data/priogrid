#' Reads the HILDA+ data
#'
#' Downloads and processes HILDA+ (Historic Land Dynamics Assessment) global
#' land use and land cover change data, filtering to years compatible with
#' PRIO-GRID temporal coverage. The function returns a multi-layer raster
#' with annual land use/land cover states.
#'
#' @details
#' HILDA+ provides global land use and land cover change data at 1 km spatial
#' resolution from 1960 to 2019. This function:
#' \itemize{
#'   \item Extracts downloaded raster files containing annual land use/land cover states
#'   \item Filters years to match PRIO-GRID temporal coverage
#'   \item Creates a multi-layer SpatRaster with standardized date names
#' }
#'
#' The land use/land cover classes in HILDA+ include various categories such as:
#' cropland, pasture, urban areas, forests, and other natural land covers.
#' Each pixel value represents a specific land use/land cover class according
#' to the HILDA+ classification scheme.
#'
#' @return A \code{SpatRaster} object (terra package) containing annual land
#'   use/land cover states. Each layer represents one year, with layer names
#'   formatted as dates (YYYY-MM-DD). The raster has:
#'   \itemize{
#'     \item Spatial resolution: 0.01 degrees (approximately 1 km)
#'     \item Coordinate reference system: WGS84 (EPSG:4326)
#'     \item Global extent: -180 to 180° longitude, -90 to 90° latitude
#'     \item Temporal coverage: Years matching PRIO-GRID dates (max 1899-2019)
#'   }
#'
#' The function depends on the internal [get_pgfile], [zip_file],
#' and [pg_dates] functions from the priogrid package.
#'
#' @seealso
#' \code{\link{pg_dates}} for PRIO-GRID temporal coverage,
#' \code{\link[terra]{rast}} for SpatRaster object details
#'
#' @examples
#' \dontrun{
#' # Read HILDA+ data
#' hilda_data <- read_hilda()
#'
#' # Examine the structure
#' print(hilda_data)
#'
#' # Get available years
#' years <- names(hilda_data)
#' print(years)
#'
#' # Extract data for a specific year (e.g., 2000)
#' hilda_2000 <- hilda_data[["2000-12-31"]]
#'
#' # Plot land use for a specific year
#' terra::plot(hilda_2000, main = "HILDA+ Land Use 2000")
#' }
#'
#' @return an object of class sf
#' @export
#'
#' @references
#' \insertRef{winklerHILDAGlobalLand2020}{priogrid}
read_hilda <- function() {
  f <- get_pgfile(source_name = "HILDA+",
                  source_version = "v1.0",
                  id = "82bc4c6f-9904-484f-aa9a-77771d076690")

  # Unzip archive
  unzipped_directory <- zip_file(f)

  # Construct the path to the raster state files directory
  states_dir <- file.path(unzipped_directory,
                          "hildap_vGLOB-1.0_geotiff_wgs84",
                          "hildap_GLOB-v1.0_lulc-states")

  # Filter raster .tif files to those matching PRIO-GRID years
  years_to_keep <- lubridate::year(pg_dates()) |> unique()
  tif_files <- list.files(states_dir, pattern = "\\.tif$", full.names = TRUE)
  years_in_files <- as.numeric(sub(".*?(\\d{4}).*", "\\1", basename(tif_files)))

  filtered_files <- tif_files[years_in_files %in% years_to_keep]
  r <- terra::rast(filtered_files)

  month_to_use <- pg_dates() |> lubridate::month() |> max()
  day_to_use <- pg_dates() |> lubridate::day() |> max()
  years_in_rast <- as.numeric(sub(".*?(\\d{4}).*", "\\1", basename(filtered_files)))
  names(r) <- as.Date(paste(years_in_rast, month_to_use, day_to_use, sep = "-"))

  return(r)
}

#' Extract Land Cover Proportions from HILDA+ Data for PRIO-GRID Cells
#'
#' Calculates the proportion of each PRIO-GRID cell covered by a specific land
#' cover type using HILDA+ global land use/land cover data. The function performs
#' spatial aggregation from high-resolution HILDA+ rasters (1km) to PRIO-GRID
#' cells (see \code{\link{pgoptions}}), accounting for partial coverage using area-weighted averaging.
#'
#' @param landcovertype Numeric. The land cover class identifier to extract from
#'   HILDA+ data. Must be a valid land cover code according to the HILDA+
#'   classification scheme (see Details for common codes).
#'
#' HILDA+ land cover class codes (for stable categories):
#' \itemize{
#'   \item 00: Ocean
#'   \item 11: Urban
#'   \item 22: Cropland
#'   \item 33: Pasture/rangeland
#'   \item 44: Forest
#'   \item 55: Unmanaged grass/shrubland
#'   \item 66: Sparse/no vegetation
#'   \item 77: Water
#'   \item 99: No data
#' }
#'
#' @return A \code{SpatRaster} object (terra package) with the same structure as
#'   PRIO-GRID, where each cell contains the proportion (0-1) of that cell covered
#'   by the specified land cover type. The raster contains:
#'   \itemize{
#'     \item Values: Proportions ranging from 0 (no coverage) to 1 (full coverage)
#'     \item Layers: One layer per year matching HILDA+ and PRIO-GRID temporal coverage
#'     \item Layer names: Dates in YYYY-MM-DD format
#'   }
#'
#' @note
#' \itemize{
#'   \item Temporary files can be created during processing but are automatically cleaned up.
#' }
#'
#' @seealso
#' \code{\link{read_hilda}} for reading raw HILDA+ data,
#' \code{\link{prio_blank_grid}} for PRIO-GRID structure,
#' \link{https://ceos.org/gst/HILDAplus.html}
#'
#' @examples
#' \dontrun{
#' # Extract cropland proportions
#' cropland_props <- hilda_landcover(landcovertype = 22)
#'
#' # View the result
#' print(cropland_props)
#'
#' # Plot cropland proportions for a specific year
#' terra::plot(cropland_props[["2000-12-31"]],
#'             main = "Cropland Proportion 2000")
#' }
#'
#' @export
#' @references
#' \insertRef{winklerHILDAGlobalLand2020}{priogrid}
hilda_landcover <- function(landcovertype){
  memfrac_option <- terra::terraOptions(verbose = FALSE)$memfrac
  terra::terraOptions(memfrac = 0.8)
  r <- read_hilda()
  res <- robust_transformation(r, function(x) mean(x == landcovertype))
  terra::terraOptions(memfrac = memfrac_option)
  return(res)
}

#' Extract Ocean Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' ocean coverage proportions (class code 00) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with ocean coverage proportions (0-1) for each
#'   PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_ocean <- function(){
  hilda_landcover(landcovertype = 00)
}

#' Extract Urban Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' urban coverage proportions (class code 11) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with urban coverage proportions (0-1) for each
#'   PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_urban <- function(){
  hilda_landcover(landcovertype = 11)
}

#' Extract Cropland Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' cropland coverage proportions (class code 22) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with cropland coverage proportions (0-1) for each
#'   PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_cropland <- function(){
  hilda_landcover(landcovertype = 22)
}

#' Extract Pasture/Rangeland Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' pasture/rangeland coverage proportions (class code 33) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with pasture/rangeland coverage proportions (0-1)
#'   for each PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_pasture <- function(){
  hilda_landcover(landcovertype = 33)
}

#' Extract Forest Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' forest coverage proportions (class code 44) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with forest coverage proportions (0-1) for each
#'   PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_forest <- function(){
  hilda_landcover(landcovertype = 44)
}

#' Extract Unmanaged Grass/Shrubland Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' unmanaged grass/shrubland coverage proportions (class code 55) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with unmanaged grass/shrubland coverage proportions
#'   (0-1) for each PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_grassland <- function(){
  hilda_landcover(landcovertype = 55)
}

#' Extract Sparse/No Vegetation Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' sparse/no vegetation coverage proportions (class code 66) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with sparse/no vegetation coverage proportions
#'   (0-1) for each PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_sparse <- function(){
  hilda_landcover(landcovertype = 66)
}

#' Extract Water Coverage from HILDA+ Data
#'
#' A convenience wrapper for \code{\link{hilda_landcover}} that extracts
#' water coverage proportions (class code 77) for PRIO-GRID cells.
#'
#' @inheritParams hilda_landcover
#' @return A \code{SpatRaster} with water coverage proportions (0-1) for each
#'   PRIO-GRID cell. See \code{\link{hilda_landcover}} for details.
#' @seealso \code{\link{hilda_landcover}} for full documentation and parameters
#' @export
gen_hilda_water <- function(){
  hilda_landcover(landcovertype = 77)
}

