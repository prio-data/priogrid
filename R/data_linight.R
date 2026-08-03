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
#'   \item Stores corrected rasters as GeoTIFF with a \code{"fixed_"} prefix
#'         for reuse
#'   \item Combines corrected rasters into a multi-layer \code{SpatRaster}
#'   \item Assigns layer names as dates, aligned to PRIO-GRID temporal
#'         conventions (the month and day are taken from the first date of
#'         \code{config})
#' }
#'
#' @section Year mapping:
#' The files are served by the Figshare API, so they are stored locally under
#' bare numeric IDs that contain neither a file extension nor a year. The year
#' of each raster therefore cannot be parsed from its filename and is instead
#' taken from its position in the source URL list
#' (\code{inst/extdata/urls/d99fbea7-2a01-4221-b900-29a58d33f591.txt}), which is
#' chronological: 22 DMSP-era files (1992-2013) followed by 11
#' simulated-VIIRS files (2014-2024). If the URL list ever changes length, the
#' function stops rather than risk mislabelling layers.
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
#' # Layer names are dates; the month/day follow the active config
#' names(linight)
#'
#' # Plot nighttime lights for year 2000 (default config)
#' terra::plot(linight[["2000-01-01"]],
#'             main = "Global Nighttime Lights 2000")
#' }
#'
#' @export
#' @references
#' \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
read_linight <- function(overwrite_files = FALSE, config = pg_current_config()){

  allfiles <- get_pgfile(source_name="Li Nighttime",
                         source_version="v10",
                         id="d99fbea7-2a01-4221-b900-29a58d33f591")

  data_dir <- dirname(allfiles[1])

  # Years covered by the v10 series, in the order the source URLs are listed.
  # See the "Year mapping" section above for why the year cannot be taken from
  # the filename.
  linight_years <- 1992:2024

  if(length(allfiles) != length(linight_years)){
    stop("Li Nighttime: expected ", length(linight_years), " files (",
         min(linight_years), "-", max(linight_years), "), found ",
         length(allfiles), ".\n",
         "  The year mapping in read_linight() must be updated to match the URL list.",
         call. = FALSE)
  }

  # Derive each harmonized raster's path from its source file, so that layer
  # order is guaranteed to match linight_years by construction.
  # NB: do not use list.files() here. Its alphabetical ordering does not match
  # the order of the URL list, which would silently attach the wrong years.
  fixed_paths <- file.path(
    data_dir,
    paste0("fixed_", tools::file_path_sans_ext(basename(allfiles)), ".tif"))

  if(overwrite_files){
    file.remove(fixed_paths[file.exists(fixed_paths)])
  }

  # Clear any temporary files left behind by an interrupted earlier run.
  unlink(list.files(data_dir, pattern = "^tmp_fixed_.*\\.tif$", full.names = TRUE))

  files_to_fix <- which(!file.exists(fixed_paths))

  if(length(files_to_fix) > 0){
    message("Harmonizing extent of Li Nighttime rasters. Next time you run the function, this will not be required")

    # Extent of many tifs are wrong, use template
    template <- terra::rast(vals = NA,
                            nrows = 21600, # Note that this is 1 cell less than original data
                            ncols = 43200, # Note that this is 1 cell less than original data
                            extent = terra::ext(c(-180, 180, -90, 90)),
                            crs = "EPSG:4326"
    )

    n <- length(files_to_fix)
    pb <- txtProgressBar(min = 0, max = n, style = 3)

    for(k in 1:n){
      setTxtProgressBar(pb, k)
      i <- files_to_fix[k]

      rsub <- terra::rast(x = allfiles[i])

      # Write to a temporary name and rename only on success, so that an
      # interrupted resample cannot leave a truncated raster under the final
      # name (which would then be treated as already harmonized).
      tmpfile <- file.path(data_dir, paste0("tmp_", basename(fixed_paths[i])))

      terra::resample(rsub, template,
                      method    = "near",
                      threads   = TRUE,
                      overwrite = TRUE,
                      progress  = FALSE,
                      filename  = tmpfile,
                      filetype  = "GTiff",            # do not rely on the extension alone
                      gdal      = c("COMPRESS=LZW"))

      file.rename(tmpfile, fixed_paths[i])
    }
    close(pb)
  }

  r <- terra::rast(fixed_paths)

  pgmonth <- pg_dates(config)[1] |> lubridate::month()
  pgday <- pg_dates(config)[1] |> lubridate::day()
  yearnames <- as.Date(paste(linight_years, pgmonth, pgday, sep = "-"))
  names(r) <- as.character(yearnames)
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
#' terra::plot(linight_pg[["2000-01-01"]],
#'             main = "PRIO-GRID Nighttime Lights (Mean, 2000)")
#'
#' # Compare mean intensity change between 2000 and 2020
#' lights_2000 <- linight_pg[["2000-01-01"]]
#' lights_2020 <- linight_pg[["2020-01-01"]]
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
