#' Reads the GHSL GHS Population Grid data
#'
#' Downloads and processes GHSL (Global Human Settlement Layer) GHS Population Grid
#' data, which provides global population estimates at high spatial resolution.
#' The function extracts multi-temporal raster files and formats them for
#' compatibility with PRIO-GRID temporal structure.
#'
#' @details
#' GHSL GHS Population Grid provides population count estimates at 1 km spatial
#' resolution for multiple time periods globally. This function:
#' \itemize{
#'   \item Downloads zipped GHSL population raster files from the data repository
#'   \item Extracts TIF files from zip archives (cached to avoid repeated extraction)
#'   \item Loads and combines multiple raster layers into a single multi-temporal raster
#'   \item Standardizes layer names using PRIO-GRID date format for consistency
#'   \item Provides population data at 5-year intervals (typically 1975-2030)
#' }
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Files are automatically extracted from zip archives and cached locally
#'   \item The function handles large files and may take time for initial download
#'   \item Population estimates include both observed and modeled/projected values
#'   \item Temporal alignment uses PRIO-GRID month/day conventions for consistency
#' }
#'
#' @examples
#' \dontrun{
#' # Read GHSL population grid data
#' # Warning: This involves large file downloads and processing
#' ghsl_population <- read_ghsl_population_grid()
#'
#' # Examine the structure
#' print(ghsl_population)
#'
#' # View available time periods
#' time_periods <- names(ghsl_population)
#' print(time_periods)
#'
#' # Plot population for specific year
#' terra::plot(ghsl_population[["2020-12-31"]],
#'             main = "Global Population Distribution 2020")
#'
#' # Compare population change over time
#' pop_1990 <- ghsl_population[["1990-12-31"]]
#' pop_2020 <- ghsl_population[["2020-12-31"]]
#' pop_change <- pop_2020 - pop_1990
#' terra::plot(pop_change, main = "Population Change 1990-2020")
#'
#' # Extract population for specific region (example: crop to extent)
#' # europe_extent <- terra::ext(-10, 40, 35, 70)
#' # europe_pop <- terra::crop(ghsl_population, europe_extent)
#' # terra::plot(europe_pop[[nlyr(europe_pop)]], main = "Europe Population")
#'
#' # Calculate total global population by year
#' global_pop_totals <- terra::global(ghsl_population, "sum", na.rm = TRUE)
#' years <- as.numeric(substr(names(ghsl_population), 1, 4))
#' plot(years, global_pop_totals$sum / 1e9,
#'      type = "l", main = "Global Population Trends",
#'      xlab = "Year", ylab = "Population (Billions)")
#' }
#'
#' @export
#' @references
#' \insertRef{schiavinaGHSPOPR2023AGHS2023}{priogrid}
read_ghsl_population_grid <- function(){
  zip_files <- get_pgfile(source_name = "GHSL GHS Population Grid",
                  source_version = "R2023",
                  id = "ae6a7612-4bef-452f-acd6-d2212cf9a7c5")

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

#' Generate GHSL GHS Population Grid
#'
#' Aggregates the high-resolution GHSL (Global Human Settlement Layer) GHS
#' Population Grid data to PRIO-GRID resolution for all available 5-year
#' intervals (1975–2030). This provides population counts per PRIO-GRID cell.
#'
#' This operation can be computationally intensive and may take time depending
#' on system performance and the size of the underlying rasters.
#'
#' A slight nearest neighbor resampling was applied to get the exact PRIO-GRID extent.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Aggregation uses sum to preserve total population counts
#'   \item Nearest-neighbor resampling is applied for spatial alignment
#'   \item Large rasters may require significant memory and processing time
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate PRIO-GRID level GHSL population rasters
#' ghsl_pg <- gen_ghsl_population_grid()
#'
#' # Inspect structure
#' print(ghsl_pg)
#'
#' # Plot population for 2020
#' terra::plot(ghsl_pg[["2020-12-31"]],
#'             main = "PRIO-GRID Population Distribution 2020")
#'
#' # Compute total population per PRIO-GRID cell change over time
#' pop_1990 <- ghsl_pg[["1990-12-31"]]
#' pop_2020 <- ghsl_pg[["2020-12-31"]]
#' change <- pop_2020 - pop_1990
#' terra::plot(change, main = "Population Change 1990–2020")
#' }
#'
#' @references
#' \insertRef{schiavinaGHSPOPR2023AGHS2023}{priogrid}
gen_ghsl_population_grid <- function(){
  r <- read_ghsl_population_grid()
  res <- robust_transformation(r, agg_fun = "sum")

  #pg <- prio_blank_grid()
  #ragg <- terra::aggregate(r, terra::res(pg)/terra::res(r), fun = "sum")
  #res <- terra::resample(ragg, pg, method = "near")

  return(res)
}
