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
#' \insertRef{crisioniPopulationDegreeUrbanization2025}{priogrid}
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
#' \insertRef{crisioniPopulationDegreeUrbanization2025}{priogrid}
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
#' \insertRef{crisioniPopulationDegreeUrbanization2025}{priogrid}
gen_ghs_wup_degurba_urban <- function(){
  ghs_wup_degurba(urban_definition = c(21, 22, 23, 30))
}
