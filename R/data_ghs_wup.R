#' Reads the GHS-WUP-DEGURBA data
#'
#' Downloads and processes GHS-WUP-DEGURBA (World Urbanization Prospects -
#' Degree of Urbanisation) data, which provides global urbanization levels
#' classified according to the Degree of Urbanisation methodology.
#' The function extracts multi-temporal raster files and formats them for
#' compatibility with PRIO-GRID temporal structure.
#'
#' @details
#' GHS-WUP-DEGURBA provides urbanization classification at 1 km spatial
#' resolution for multiple time periods globally. This function:
#' \itemize{
#'   \item Downloads zipped GHS-WUP-DEGURBA raster files from the data repository
#'   \item Extracts TIF files from zip archives (cached to avoid repeated extraction)
#'   \item Loads and combines multiple raster layers into a single multi-temporal raster
#'   \item Standardizes layer names using PRIO-GRID date format for consistency
#'   \item Provides urbanization classification data at 5-year intervals (typically 1975-2030)
#' }
#'
#' The DEGURBA classification uses the following codes:
#' \itemize{
#'   \item 30: Urban centres (cities)
#'   \item 23: Dense urban cluster
#'   \item 22: Semi-dense urban cluster
#'   \item 21: Suburban or peri-urban
#'   \item 13: Rural cluster
#'   \item 12: Low density rural
#'   \item 11: Very low density rural
#'   \item 10: Water bodies or uninhabited areas
#' }
#'
#' @return A \code{SpatRaster} object with DEGURBA classification codes
#'
#' @note
#' \itemize{
#'   \item Files are automatically extracted from zip archives and cached locally
#'   \item The function handles large files and may take time for initial download
#'   \item Classification includes both observed and modeled/projected values
#'   \item Temporal alignment uses PRIO-GRID month/day conventions for consistency
#' }
#'
#' @examples
#' \dontrun{
#' # Read GHS-WUP-DEGURBA data
#' # Warning: This involves large file downloads and processing
#' degurba <- read_ghs_wup_degurba()
#'
#' # Examine the structure
#' print(degurba)
#'
#' # View available time periods
#' time_periods <- names(degurba)
#' print(time_periods)
#'
#' # Plot urbanization classification for specific year
#' terra::plot(degurba[["2020-12-31"]],
#'             main = "Global Urbanization Classification 2020")
#'
#' # Compare urbanization change over time
#' urban_1990 <- degurba[["1990-12-31"]]
#' urban_2020 <- degurba[["2020-12-31"]]
#' terra::plot(urban_2020 - urban_1990,
#'             main = "Urbanization Change 1990-2020")
#'
#' # Extract urbanization for specific region (example: crop to extent)
#' # asia_extent <- terra::ext(60, 140, -10, 50)
#' # asia_urban <- terra::crop(degurba, asia_extent)
#' # terra::plot(asia_urban[[nlyr(asia_urban)]], main = "Asia Urbanization")
#'
#' # Calculate frequency of each urbanization class
#' freq_2020 <- terra::freq(degurba[["2020-12-31"]])
#' print(freq_2020)
#' }
#'
#' @references
#' \insertRef{schiavinaGHSWUPDEGURBAR2025AGHSWUP2025}{priogrid}
#'
#' \insertRef{europeancommissionApplyingDegreeUrbanisation2021}{priogrid}
#'
#' \insertRef{jacobs-crisioniPopulationDegreeUrbanization2025}{priogrid}
read_ghs_wup_degurba <- function(){
  zip_files <- get_pgfile(source_name = "GHS-WUP-DEGURBA",
                          source_version = "R2025A",
                          id = "7f1f60a3-6664-4427-b086-b5359ebf45b7")

  # unzip(zip_files[1], list = T)
  tif_files <- basename(zip_files) |> tools::file_path_sans_ext() |> paste0(".tif")

  for(i in 1:length(zip_files)){
    if(!file.exists(file.path(dirname(zip_files[i]), tif_files[i]))){
      unzip(zip_files[i], files = tif_files[i], exdir = dirname(zip_files[i]))
    }
  }

  r <- terra::rast(file.path(dirname(zip_files[i]), tif_files[1]))
  for(i in 2:length(tif_files)){
    terra::add(r) <- terra::rast(file.path(dirname(zip_files[i]), tif_files[i]))
  }

  pgmonth <- pg_dates()[1] |> lubridate::month()
  pgday <- pg_dates()[1] |> lubridate::day()
  tif_dates <- stringr::str_extract(tif_files, seq(1975, 2030, by = 5) |> paste(collapse = "|"))
  tif_dates <- lubridate::ymd(paste(tif_dates, pgmonth, pgday, sep = "-")) |> as.character()

  names(r) <- tif_dates
  return(r)
}


#' Apply urban classification to GHS-WUP-DEGURBA data
#'
#' Reclassifies the GHS-WUP-DEGURBA data into binary urban/non-urban categories
#' based on a user-defined set of DEGURBA classification codes. The function
#' aggregates the resulting binary classification to PRIO-GRID resolution,
#' providing the proportion of urban area within each PRIO-GRID cell.
#'
#' @param urban_definition Numeric vector of DEGURBA codes to classify as urban.
#'   Valid codes are: 10, 11, 12, 13, 21, 22, 23, 30. See Details for code meanings.
#'
#' @details
#' The DEGURBA classification codes represent:
#' \itemize{
#'   \item 30: Urban centres (cities)
#'   \item 23: Dense urban cluster
#'   \item 22: Semi-dense urban cluster
#'   \item 21: Suburban or peri-urban
#'   \item 13: Rural cluster
#'   \item 12: Low density rural
#'   \item 11: Very low density rural
#'   \item 10: Water bodies or uninhabited areas
#' }
#'
#' The function performs the following operations:
#' \itemize{
#'   \item Reads the high-resolution GHS-WUP-DEGURBA data
#'   \item Reclassifies cells to 1 (urban) if they match \code{urban_definition}, 0 otherwise
#'   \item Aggregates to PRIO-GRID resolution using mean (proportion of urban area)
#'   \item Applies nearest-neighbor resampling for exact PRIO-GRID alignment
#' }
#'
#' A slight nearest neighbor resampling was applied to get the exact PRIO-GRID extent.
#'
#' @return A \code{SpatRaster} object with values ranging from 0 to 1, representing
#'   the proportion of urban area within each PRIO-GRID cell according to the
#'   specified urban definition.
#'
#' @note
#' \itemize{
#'   \item This operation can be computationally intensive for large rasters
#'   \item Temporary files are created during processing
#'   \item The aggregation uses mean to calculate urban proportion per cell
#'   \item Different urban definitions produce different urbanization estimates
#' }
#'
#' @examples
#' \dontrun{
#' # Use only urban centres as urban definition
#' urban_strict <- ghs_wup_degurba(urban_definition = 30)
#'
#' # Use broader urban definition (all urban and suburban areas)
#' urban_broad <- ghs_wup_degurba(urban_definition = c(21, 22, 23, 30))
#'
#' # Plot comparison
#' terra::plot(urban_strict[["2020-12-31"]],
#'             main = "Strict Urban Definition (Centres Only)")
#' terra::plot(urban_broad[["2020-12-31"]],
#'             main = "Broad Urban Definition")
#'
#' # Custom definition: exclude suburban areas
#' urban_custom <- ghs_wup_degurba(urban_definition = c(22, 23, 30))
#' }
#'
#' @references
#' \insertRef{schiavinaGHSWUPDEGURBAR2025AGHSWUP2025}{priogrid}
#'
#' \insertRef{europeancommissionApplyingDegreeUrbanisation2021}{priogrid}
#'
#' \insertRef{jacobs-crisioniPopulationDegreeUrbanization2025}{priogrid}
ghs_wup_degurba <- function(urban_definition){
  cl_mat <- cbind(c(10, 11, 12, 13, 21, 22, 23, 30),
                  rep(0, 8))
  cl_mat[,2] <- ifelse(cl_mat[,1] %in% urban_definition, 1, 0)
  r <- read_ghs_wup_degurba()

  temporary_directory <- file.path(pgoptions$get_rawfolder(), "tmp", tempdir() |> basename())
  dir.create(temporary_directory)
  tmp1 <- tempfile(pattern = "urban_classification", fileext = ".tif", tmpdir = temporary_directory)

  r <- terra::classify(r, cl_mat, filename = tmp1)
  res <- robust_transformation(r, agg_fun = "mean")
}

#' Generate GHS-WUP-DEGURBA urban proportion grid
#'
#' Generates PRIO-GRID level urbanization data using the GHS-WUP-DEGURBA
#' (Degree of Urbanisation) classification with a standard urban definition.
#' This function provides the proportion of urban area within each PRIO-GRID
#' cell for all available 5-year intervals (1975–2030).
#'
#' @details
#' This function uses a predefined urban definition that includes:
#' \itemize{
#'   \item 30: Urban centres (cities)
#'   \item 23: Dense urban cluster
#'   \item 22: Semi-dense urban cluster
#'   \item 21: Suburban or peri-urban
#' }
#'
#' Areas classified as rural (codes 10, 11, 12, 13) are considered non-urban.
#' The resulting raster provides values between 0 and 1, representing the
#' proportion of each PRIO-GRID cell that is classified as urban.
#'
#' This operation can be computationally intensive and may take time depending
#' on system performance and the size of the underlying rasters.
#'
#' A slight nearest neighbor resampling was applied to get the exact PRIO-GRID extent.
#'
#' @return A \code{SpatRaster} object with values ranging from 0 to 1
#'
#' @note
#' \itemize{
#'   \item Aggregation uses mean to calculate urban proportion per cell
#'   \item Nearest-neighbor resampling is applied for spatial alignment
#'   \item Large rasters may require significant memory and processing time
#'   \item For custom urban definitions, use \code{\link{ghs_wup_degurba}} directly
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate PRIO-GRID level urbanization rasters
#' urban_pg <- gen_ghs_wup_degurba_urban()
#'
#' # Inspect structure
#' print(urban_pg)
#'
#' # Plot urbanization for 2020
#' terra::plot(urban_pg[["2020-12-31"]],
#'             main = "PRIO-GRID Urban Proportion 2020")
#'
#' # Compute urbanization change over time
#' urban_1990 <- urban_pg[["1990-12-31"]]
#' urban_2020 <- urban_pg[["2020-12-31"]]
#' change <- urban_2020 - urban_1990
#' terra::plot(change, main = "Urbanization Change 1990–2020")
#'
#' # Identify highly urbanized cells (>75% urban)
#' highly_urban <- urban_pg[["2020-12-31"]] > 0.75
#' terra::plot(highly_urban, main = "Highly Urbanized Areas")
#'
#' # Calculate global urban area trends
#' urban_stats <- terra::global(urban_pg, "mean", na.rm = TRUE)
#' years <- as.numeric(substr(names(urban_pg), 1, 4))
#' plot(years, urban_stats$mean,
#'      type = "l", main = "Global Urbanization Trends",
#'      xlab = "Year", ylab = "Mean Urban Proportion")
#' }
#'
#' @seealso \code{\link{ghs_wup_degurba}} for custom urban definitions
#'
#' @references
#' \insertRef{schiavinaGHSWUPDEGURBAR2025AGHSWUP2025}{priogrid}
#'
#' \insertRef{europeancommissionApplyingDegreeUrbanisation2021}{priogrid}
#'
#' \insertRef{jacobs-crisioniPopulationDegreeUrbanization2025}{priogrid}
gen_ghs_wup_degurba_urban <- function(){
  ghs_wup_degurba(urban_definition = c(21, 22, 23, 30))
}


#' Extract contiguous urban extent around a location
#'
#' Identifies and delineates the contiguous urban extent surrounding a specified
#' geographic location using GHS-WUP-DEGURBA (Degree of Urbanisation) data.
#' The function extracts the urban patch that contains the input coordinates,
#' providing a polygon boundary of the connected urban area.
#'
#' @param lon Numeric. Longitude of the location of interest (WGS84, decimal degrees).
#' @param lat Numeric. Latitude of the location of interest (WGS84, decimal degrees).
#' @param measurement_date Character. Date for which to extract urban extent,
#'   in format "YYYY-MM-DD" (e.g., "2020-12-31"). Must match one of the available
#'   dates in the GHS-WUP-DEGURBA dataset (typically at 5-year intervals:
#'   1975, 1980, ..., 2030).
#' @param urban_definition Numeric vector of DEGURBA codes to classify as urban.
#'   Default is \code{c(21, 22, 23, 30)} (suburban, semi-dense, dense, and urban centres).
#'   Valid codes are: 10, 11, 12, 13, 21, 22, 23, 30. See Details for code meanings.
#' @param max_extent Numeric. Maximum search radius in meters around the input
#'   coordinates. Default is 1000000 (1000 km). This limits the area searched
#'   for connected urban patches and improves computational efficiency.
#'
#' @details
#' The function performs the following workflow:
#' \enumerate{
#'   \item Creates a circular buffer of radius \code{max_extent} around the input coordinates
#'   \item Extracts GHS-WUP-DEGURBA data for the specified \code{measurement_date}
#'   \item Crops the raster to the buffer area for computational efficiency
#'   \item Reclassifies cells to binary urban (1) or non-urban (0) based on \code{urban_definition}
#'   \item Identifies all contiguous urban patches using 8-directional connectivity
#'   \item Determines which patch contains the input coordinates
#'   \item Extracts and dissolves the contiguous urban patch into a single polygon
#' }
#'
#' The DEGURBA classification codes represent:
#' \itemize{
#'   \item 30: Urban centres (cities)
#'   \item 23: Dense urban cluster
#'   \item 22: Semi-dense urban cluster
#'   \item 21: Suburban or peri-urban
#'   \item 13: Rural cluster
#'   \item 12: Low density rural
#'   \item 11: Very low density rural
#'   \item 10: Water bodies or uninhabited areas
#' }
#'
#' **Contiguity**: Urban patches are defined using 8-directional connectivity,
#' meaning cells are considered connected if they share an edge or corner.
#'
#' @return An \code{sf} polygon object representing the contiguous urban extent
#'   containing the input location. The polygon is returned in the coordinate
#'   reference system specified by \code{pgoptions$get_crs()}. If the input
#'   location is not classified as urban, the function may return an empty
#'   polygon or fail.
#'
#' @note
#' \itemize{
#'   \item The function requires the input coordinates to fall within an urban area
#'   \item Larger \code{max_extent} values increase computational time and memory usage
#'   \item The urban definition significantly affects the resulting extent
#'   \item Stricter definitions (e.g., only code 30) produce smaller extents
#'   \item Broader definitions (e.g., codes 21-30) produce larger extents
#'   \item Processing time varies with the size of the urban area
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract urban extent for Oslo, Norway (2020)
#' oslo_extent <- urban_extent(
#'   lon = 10.763063,
#'   lat = 59.935320,
#'   measurement_date = "2020-12-31"
#' )
#'
#' # Plot the result
#' plot(sf::st_geometry(oslo_extent), main = "Oslo Urban Extent 2020")
#'
#' # Extract urban extent for New Delhi, India (2020)
#' delhi_extent <- urban_extent(
#'   lon = 77.231487,
#'   lat = 28.612738,
#'   measurement_date = "2020-12-31"
#' )
#'
#' # Compare urban extent over time for the same location
#' paris_1975 <- urban_extent(
#'   lon = 2.3522,
#'   lat = 48.8566,
#'   measurement_date = "1975-12-31"
#' )
#' paris_2020 <- urban_extent(
#'   lon = 2.3522,
#'   lat = 48.8566,
#'   measurement_date = "2020-12-31"
#' )
#'
#' # Calculate urban area expansion
#' area_1975 <- sf::st_area(paris_1975)
#' area_2020 <- sf::st_area(paris_2020)
#' expansion <- (area_2020 - area_1975) / area_1975 * 100
#' cat("Urban expansion:", round(expansion, 1), "%\n")
#'
#' # Use stricter urban definition (only urban centres)
#' tokyo_core <- urban_extent(
#'   lon = 139.6917,
#'   lat = 35.6895,
#'   measurement_date = "2020-12-31",
#'   urban_definition = 30
#' )
#'
#' # Use broader urban definition (including all urbanized areas)
#' tokyo_metro <- urban_extent(
#'   lon = 139.6917,
#'   lat = 35.6895,
#'   measurement_date = "2020-12-31",
#'   urban_definition = c(21, 22, 23, 30)
#' )
#'
#' # Reduce search radius for smaller cities
#' kongsvinger <- urban_extent(
#'   lon = 12.000669,
#'   lat = 60.190700,
#'   measurement_date = "2020-12-31",
#'   max_extent = 50e3  # 50 km radius
#' )
#'
#' # Extract multiple cities and compare
#' cities <- data.frame(
#'   name = c("London", "Paris", "Berlin"),
#'   lon = c(-0.1276, 2.3522, 13.4050),
#'   lat = c(51.5074, 48.8566, 52.5200)
#' )
#'
#' city_extents <- lapply(1:nrow(cities), function(i) {
#'   urban_extent(
#'     lon = cities$lon[i],
#'     lat = cities$lat[i],
#'     measurement_date = "2020-12-31"
#'   )
#' })
#'
#' # Calculate areas
#' city_areas <- sapply(city_extents, sf::st_area)
#' cities$area_km2 <- as.numeric(city_areas) / 1e6
#' print(cities)
#' }
#'
#' @seealso
#' \code{\link{read_ghs_wup_degurba}} for reading the underlying DEGURBA data
#'
#' \code{\link{ghs_wup_degurba}} for custom urban classification
#'
#' @references
#' \insertRef{schiavinaGHSWUPDEGURBAR2025AGHSWUP2025}{priogrid}
#'
#' \insertRef{europeancommissionApplyingDegreeUrbanisation2021}{priogrid}
urban_extent <- function(lon, lat, measurement_date, urban_definition = c(21, 22, 23, 30), max_extent = 1000e3){
  # lat <- 59.935320; lon <- 10.763063 Oslo
  # lat <- 28.612738 lon <- 77.231487 New Delhi

  urban_centre <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  urban_buffer <- sf::st_buffer(urban_centre, max_extent)


  r <- read_ghs_wup_degurba()
  urban_buffer <- sf::st_transform(urban_buffer, crs = terra::crs(r))
  urban_centre <- sf::st_transform(urban_centre, crs = terra::crs(r))
  r <- terra::subset(r, which(names(r) == measurement_date))
  r <- terra::crop(r, urban_buffer)

  cl_mat <- cbind(c(10, 11, 12, 13, 21, 22, 23, 30),
                  rep(0, 8))
  cl_mat[,2] <- ifelse(cl_mat[,1] %in% urban_definition, 1, 0)
  urban_binary <- terra::classify(r, cl_mat)
  center_cell <- terra::extract(urban_binary, terra::vect(urban_centre), cells = TRUE)$cell

  urban_patches <- terra::patches(urban_binary, directions = 8, zeroAsNA=TRUE)
  center_patch_id <- terra::values(urban_patches)[center_cell]
  urban_extent_raster <- terra::ifel(urban_patches == center_patch_id, 1, NA)
  urban_extent_raster <- terra::trim(urban_extent_raster)
  res <- terra::as.polygons(urban_extent_raster, dissolve = TRUE) |> sf::st_as_sf()
  res <- sf::st_transform(res, pgoptions$get_crs())
  res
}
