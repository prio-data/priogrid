#' Reads the Li Nighttime data
#'
#' Downloads, preprocesses, and harmonizes the Li et al. global nighttime
#' light dataset (v10). This dataset provides global, annual composites
#' of nighttime light intensity, harmonized across multiple satellite
#' sensors to produce a consistent multi-decadal time series.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Downloads individual Li Nighttime Lights raster files from the
#'         PRIO-GRID data repository via the Figshare API
#'   \item Identifies rasters with extent mismatches (common in the dataset)
#'   \item Resamples problematic rasters to a standardized global template
#'         (\code{EPSG:4326}, extent -180/180, -90/90) using nearest neighbor
#'         resampling
#'   \item Stores corrected rasters with a \code{"extentfixed_"} prefix for reuse
#'   \item Combines corrected rasters into a multi-layer \code{SpatRaster}
#'   \item Assigns layer names as dates, aligned to PRIO-GRID temporal
#'         conventions
#' }
#'
#' @param overwrite_files Logical. If \code{TRUE}, previously fixed rasters are
#'   recalculated and overwritten. Defaults to \code{FALSE}.
#' @param config A \code{pg_config} object. Defaults to \code{\link{pg_current_config}()}.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Initial preprocessing (extent harmonization) may take time, but is
#'         cached for faster subsequent runs
#'   \item Large raster files may require substantial disk space and memory
#'   \item Nighttime lights are influenced by sensor calibration, atmospheric
#'         conditions, and moonlight; Li et al. provide harmonization but
#'         residual inconsistencies may remain
#' }
#'
#' @examples
#' \dontrun{
#' # Read harmonized Li Nighttime Lights data
#' linight <- read_linight()
#'
#' # Inspect structure
#' print(linight)
#'
#' # Plot nighttime lights for year 2000
#' terra::plot(linight[["2000-12-31"]],
#'             main = "Global Nighttime Lights 2000")
#'
#' # Compare change between 2000 and 2020
#' lights_2000 <- linight[["2000-12-31"]]
#' lights_2020 <- linight[["2020-12-31"]]
#' change <- lights_2020 - lights_2000
#' terra::plot(change, main = "Nighttime Lights Change 2000–2020")
#'
#' # Extract regional time series
#' # example_extent <- terra::ext(100, 120, 20, 40) # East Asia
#' # region_lights <- terra::crop(linight, example_extent)
#' # terra::plot(region_lights[[1]], main = "Regional Nighttime Lights")
#' }
#'
#' @export
#' @references
#' \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
read_linight <- function(config = pg_current_config(), overwrite_files = FALSE){

  allfiles <- get_pgfile(source_name="Li Nighttime",
                         source_version="v10",
                         id="d99fbea7-2a01-4221-b900-29a58d33f591")

  years <- basename(allfiles) |> stringr::str_extract(pattern = "\\d{4}") |> as.integer()

  extentfixed_files <- list.files(dirname(allfiles[1]), pattern = "^extentfix_", full.names = TRUE)
  files_to_fix <- allfiles[!file.exists(extentfixed_files)]

  if(length(files_to_fix) > 0){
    message("Harmonizing extent of Li Nighttime rasters. Next time you run the function, this will not be required")
    pg <- prio_blank_grid(config = config)
    template <- terra::rast(vals = NA,
                            nrows = 21600, # Note that this is 1 cell less than original data
                            ncols = 43200, # Note that this is 1 cell less than original data
                            extent = terra::ext(c(-180, 180, -90, 90)),
                            crs = "EPSG:4326"
    )

    for(f in files_to_fix){
      r_orig <- terra::rast(f)
      fname <- paste0("extentfix_", basename(f))
      terra::resample(r_orig, template, method = "near", threads = T, overwrite = overwrite_files, filename = file.path(dirname(f), fname))
    }
  }

  r <- terra::rast(extentfixed_files)

  pgmonth <- pg_dates(config)[1] |> lubridate::month()
  pgday <- pg_dates(config)[1] |> lubridate::day()
  full_dates <- as.Date(paste(years, pgmonth, pgday, sep = "-"))
  names(r) <- full_dates
  return(r)
}

#' Generate Li Nighttime Light
#'
#' Aggregates the high-resolution Li et al. harmonized global nighttime lights
#' dataset to PRIO-GRID resolution for all available years (1992–2024).
#' This produces PRIO-GRID cell-level averages of nighttime light intensity,
#' harmonized with PRIO-GRID’s spatial and temporal structure.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Reads annual nighttime lights rasters via \code{\link{read_linight}}
#'   \item Aggregates 1 km nighttime light intensity values into PRIO-GRID
#'         cells using mean values
#'   \item Retains global temporal coverage (1992–2024) as a multi-layer
#'         \code{SpatRaster}
#'   \item Aligns precisely to PRIO-GRID spatial extent (resampling handled
#'         in \code{\link{read_linight}})
#' }
#'
#' @param config A \code{pg_config} object. Defaults to \code{\link{pg_current_config}()}.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Aggregation uses mean values to represent typical nighttime light
#'         intensity per PRIO-GRID cell
#'   \item For sum-based aggregation (e.g., total light output per cell),
#'         see \code{\link{robust_transformation}} with \code{agg_fun = "sum"}
#'   \item Large rasters may take time and memory to process
#' }
#'
#' @examples
#' \dontrun{
#' # Generate PRIO-GRID level Li Nighttime Lights data
#' linight_pg <- gen_linight_mean()
#'
#' # Inspect structure
#' print(linight_pg)
#'
#' # Plot mean nighttime lights for 2000
#' terra::plot(linight_pg[["2000-12-31"]],
#'             main = "PRIO-GRID Nighttime Lights (Mean, 2000)")
#'
#' # Compare mean intensity change between 2000 and 2020
#' lights_2000 <- linight_pg[["2000-12-31"]]
#' lights_2020 <- linight_pg[["2020-12-31"]]
#' change <- lights_2020 - lights_2000
#' terra::plot(change, main = "Change in Mean Nighttime Lights 2000–2020")
#' }
#'
#' @export
#' @references
#' \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
gen_linight_mean <- function(config = pg_current_config()){
  r <- read_linight(config = config)

  res <- robust_transformation(r, agg_fun = "mean", config = config)

  return(res)
}

