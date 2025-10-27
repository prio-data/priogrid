#------------------------------------------------------------------------------
# FUNCTION 1:
#------------------------------------------------------------------------------
#' Read and Unpack Global Landcover (GLC-FCS30 v2) Source Files
#'
#' Retrieves and prepares raw Global Landcover (GLC-FCS30 v2) data tiles for analysis.
#' This function locates, optionally unzips, and lists all annual raster files
#' (.tif format) from the ESA GLC-FCS30 v2 dataset. It supports partial loading for
#' testing or memory-constrained environments.
#'
#' @details
#' The function automates the initial ingestion and unpacking of the GLC-FCS30 dataset:
#' \enumerate{
#'   \item Uses \code{get_pgfile()} to identify the local or network dataset directory
#'         for the specified GLC version (currently \code{"GLC_FCS30"} v2).
#'   \item Lists all available \code{.zip} archives within the dataset directory.
#'   \item Optionally limits the number of archives processed when \code{beta_test = TRUE}
#'         or when \code{foldersize} is specified.
#'   \item Extracts each \code{.zip} archive into a matching folder if not already unzipped.
#'   \item Collects paths to all valid annual \code{.tif} raster files, excluding
#'         5-year composites (filenames containing "5years").
#' }
#'
#' This prepares the raster file list required for downstream preprocessing via
#' \code{\link{prepare_glc_layers()}} and aggregation using
#' \code{\link{glc_landcover()}} or \code{\link{robust_glc_landcover()}}.
#'
#' @note
#' - The function assumes a consistent directory structure produced by ESA GLC-FCS30 v2.
#' - Existing unzipped folders are skipped to avoid redundant extraction.
#' - Only annual layers are returned; 5-year composite rasters are excluded automatically.
#' - If \code{beta_test = TRUE}, the function processes only the first \code{foldersize}
#'   ZIP archives, allowing rapid functional testing on limited subsets.
#'
#' @param beta_test Logical; if TRUE, activates test mode by restricting the number
#'   of ZIP files unzipped and processed.
#' @param foldersize Optional integer defining how many ZIP folders to include.
#'   Defaults to all available archives.
#'
#' @return A character vector of file paths pointing to annual \code{.tif} raster files
#'   from the unzipped GLC-FCS30 v2 dataset.
#'
#' @seealso
#' \code{\link{get_pgfile}} for dataset path retrieval,
#' \code{\link{prepare_glc_layers}} for temporal filtering and naming,
#' \code{\link{glc_landcover}} and \code{\link{robust_glc_landcover}} for aggregation.
#'
#' @examples
#' # Example 1: Load all available GLC-FCS30 v2 tiles
#' # tif_files <- read_glc_v2()
#'
#' # Example 2: Beta test mode (first 2 ZIP files)
#' # tif_files <- read_glc_v2(beta_test = TRUE, foldersize = 2)
#'
#' @export
#'
read_glc_v2 <- function(beta_test = FALSE, foldersize = NULL) {

  # Base dataset directory
  #f <- file.path(
  #  "/Volumes/T7/PRIOGRID",
  #  "GLC_FCS30",
  #  "v2",
  #  "7f03a296-4329-4458-8b62-83c3d27530af"
  #)



  f <- get_pgfile(source_name = "GLC_FCS30",
                    source_version = "v2",
                    id = "7f03a296-4329-4458-8b62-83c3d27530af")

  dir <- dirname(f[1])  # just grab the directory of the first file
  zips <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)

  # Default foldersize = all zip files
  if (is.null(foldersize)) foldersize <- length(zips)

  # Optional beta test subset
  if (beta_test) {
    message("Running in beta test mode: limiting to first ", foldersize, " zip file(s).")
    zips <- head(zips, foldersize)
  }

  # ---- Unzip only if not already unzipped ----
  unzipped_dirs <- vapply(zips, function(z) {
    target_dir <- sub("\\.zip$", "", z)
    if (!dir.exists(target_dir)) {
      message("Unzipping ", basename(z))
      unzip(z, exdir = target_dir)
    } else {
      message("Skipping (already unzipped): ", basename(z))
    }
    return(target_dir)
  }, FUN.VALUE = character(1))

  # ---- Gather all annual (not 5-year) TIFFs ----
  tif_files <- list.files(
    unzipped_dirs,
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  )
  tif_files <- tif_files[!grepl("5years", basename(tif_files), ignore.case = TRUE)]

  return(tif_files)

}

#------------------------------------------------------------------------------
# FUNCTION 2:
#------------------------------------------------------------------------------
#' Prepare Global Landcover (GLC) Raster Layers for PRIO-GRID Aggregation
#'
#' Reads, filters, and renames high-resolution Global Landcover (GLC) raster tiles
#' prior to aggregation. Each 5×5° tile typically contains multiple annual layers
#' (e.g., 1992–2020 composites). This function standardizes their naming conventions,
#' aligns their temporal coverage with PRIO-GRID reference years, and outputs a
#' list of cleaned \code{SpatRaster} objects ready for spatial aggregation.
#'
#' @details
#' The function automates several preprocessing steps:
#' \enumerate{
#'   \item Reads all available GLC TIFF tiles using \code{read_glc_v2()}.
#'   \item Optionally limits the number of tiles loaded via \code{n_tifs} for testing.
#'   \item Extracts year ranges from file names and safely renames layers.
#'   \item Filters layers to match the temporal range defined by \code{pg_dates()}.
#'   \item Converts layer names to standardized ISO date format (\code{YYYY-MM-DD})
#'         for interoperability with time-series analysis workflows.
#' }
#'
#' This function ensures that only temporally valid and correctly named raster layers
#' are passed to \code{glc_landcover()} or \code{robust_glc_landcover()}. It is designed
#' to minimize memory use and provide clear diagnostics on mismatched years.
#'
#' @param beta_test Logical; if TRUE, runs in test mode and may load a reduced number of tiles.
#' @param foldersize Optional integer specifying the batch size for reading tiles
#'   (used to control memory usage during large-scale operations).
#' @param n_tifs Optional integer limiting how many TIFF files are processed, typically
#'   for debugging or lightweight validation.
#'
#' @return A named list of filtered \code{SpatRaster} objects. Each raster contains
#' annual layers with standardized names corresponding to valid PRIO-GRID years.
#'
#' @seealso
#' \code{\link{read_glc_v2}} for loading raw GLC files,
#' \code{\link{glc_landcover}} and \code{\link{robust_glc_landcover}} for aggregation.
#'
#' @examples
#' # Load and filter ESA GLC raster tiles (test mode)
#' # rasters <- prepare_glc_layers(beta_test = TRUE, n_tifs = 2)
#'
#' # Example: Extract layer years from the first tile
#' # names(rasters[[1]])
#'
#' @export

prepare_glc_layers <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  library(terra)
  library(lubridate)

  # ---- Step 1: Read TIFF files from GLC folders ----
  tif_files <- read_glc_v2(beta_test = beta_test, foldersize = foldersize)

  message("Found ", length(tif_files), " TIFF files to process.\n")

  # ---- Optional Step: Limit to first N TIFFs ----
  if (!is.null(n_tifs)) {
    if (n_tifs < length(tif_files)) {
      message("Limiting to first ", n_tifs, " TIFF files for testing.")
      tif_files <- head(tif_files, n_tifs)
    } else {
      message("Requested n_tifs (", n_tifs, ") >= total available TIFFs; using all files.")
    }
  }

  # ---- Load and rename rasters by year range ----
  ras_list <- vector("list", length(tif_files))

  for (i in seq_along(tif_files)) {
    r_i <- rast(tif_files[i])
    fname <- basename(tif_files[i])

    # Parse start and end years from filename
    start_year <- as.numeric(sub(".*_(\\d{4})\\d{4}.*", "\\1", fname))
    end_year   <- as.numeric(sub(".*_(\\d{8}).*", "\\1", fname))
    end_year   <- as.numeric(substr(end_year, 5, 8))

    # Build the sequence of years
    years <- seq(start_year, end_year)

    # Rename layers safely
    names(r_i) <- years[seq_len(min(length(years), nlyr(r_i)))]

    ras_list[[i]] <- r_i
  }

  # ---- Step 2: Filter layers by PRIO-GRID years ----
  years_to_keep <- year(pg_dates()) |> unique()

  ras_list_filtered <- lapply(ras_list, function(r) {
    raw_names <- names(r)
    layer_years <- suppressWarnings(as.numeric(gsub("\\D", "", raw_names)))
    keep_layers <- layer_years %in% years_to_keep

    if (any(keep_layers)) {
      r <- r[[keep_layers]]
      names(r) <- layer_years[keep_layers]
    } else {
      warning("No matching years found for raster: ", sources(r))
      r <- NULL
    }
    return(r)
  })

  # Remove NULL rasters
  ras_list_filtered <- Filter(Negate(is.null), ras_list_filtered)

  # ---- Step 3: Rename to full YYYY-MM-DD format ----
  month_to_use <- pg_dates() |> month() |> max()
  day_to_use   <- pg_dates() |> day()   |> max()

  ras_list_filtered <- lapply(ras_list_filtered, function(r) {
    yrs <- as.numeric(names(r))
    names(r) <- as.Date(paste(yrs, month_to_use, day_to_use, sep = "-"))
    return(r)
  })

  # ---- Return processed rasters ----
  message("Layer preparation complete. Returning filtered raster list.")
  return(ras_list_filtered)
}

#------------------------------------------------------------------------------
# FUNCTION 3:
#------------------------------------------------------------------------------

#' Robust Landcover Aggregation Function
#'
#' GLC land cover class codes (for stable categories):
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
#' Aggregates high-resolution Global Landcover (GLC) tiles into PRIO-GRID scale rasters.
#' Each 5×5° tile is processed independently, layer-by-layer, using a robust and memory-safe
#' transformation that automatically aligns PRIO-GRID extents with the tile extent.
#'
#' @details
#' This function extends \code{glc_landcover()} by incorporating additional error handling
#' and memory management features. It is particularly suitable for large-scale or distributed
#' processing tasks, where tile extents may vary slightly or where raster alignment mismatches
#' could cause resampling artifacts. The internal use of \code{tiled = TRUE} ensures each
#' PRIO-GRID cell is aggregated only within the spatial footprint of its corresponding GLC tile.
#'
#' @note
#' Input rasters are expected to use ESA WorldCover classification codes (0–250).
#' Filled values (0, 250) are ignored during aggregation. The function assumes
#' all tiles share the same CRS and pixel alignment as defined in \code{prepare_glc_layers()}.
#'
#' @seealso
#' \code{\link{glc_landcover}}, \code{\link{prepare_glc_layers}}, \code{\link{robust_transformation}}
#'
#' @param landcovertype Integer or vector of integer GLC codes representing the landcover
#'   classes of interest (e.g., cropland = c(10, 11, 12)).
#' @inheritParams prepare_glc_layers
#' @return A SpatRaster of PRIO-GRID aggregated landcover fractions (0–1) per cell.
#' @export
#'
#' @examples
#' # Example: Compute shrubland coverage
#' # shrubland <- robust_glc_landcover(c(120, 121, 122, 150, 152))
#'
robust_glc_landcover <- function(landcovertype, beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {

  # -------------------------------------------------------------------------
  # STEP 1: Prepare and load GLC raster tiles
  # -------------------------------------------------------------------------
  # Each tile typically covers a 5×5° geographic area.
  # The prepare_glc_layers() function returns a list of SpatRaster objects,
  # where each tile contains multiple time layers (e.g., annual composites).
  # -------------------------------------------------------------------------
  ras_list_filtered <- prepare_glc_layers(beta_test, foldersize, n_tifs)


  # -------------------------------------------------------------------------
  # STEP 2: Define block-level aggregation function
  # -------------------------------------------------------------------------
  # The block_fun determines how the fine-resolution pixels (30m)
  # are summarized into a single PRIO-GRID cell (~55km).
  #
  # - Each "block" corresponds to one PRIO cell footprint.
  # - The function calculates the proportion of pixels that match
  #   the target landcover type(s) defined in `landcovertype`.
  #
  # The result for each PRIO cell will be a number between 0 and 1.
  # -------------------------------------------------------------------------
  block_fun <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    mean(x %in% landcovertype, na.rm = TRUE)
  }


  # -------------------------------------------------------------------------
  # STEP 3: Process each raster tile independently
  # -------------------------------------------------------------------------
  # Each tile is processed layer-by-layer (typically one layer per year).
  # The function robust_transformation() handles the heavy lifting:
  # it performs aggregation to PRIO-GRID scale and ensures numerical stability.
  #
  # The key improvement here is `tiled = TRUE`, which instructs
  # robust_transformation() to crop PRIO-GRID to match the tile extent.
  # This avoids global resampling and prevents extent/alignment errors.
  # -------------------------------------------------------------------------
  results <- lapply(seq_along(ras_list_filtered), function(i) {
    tile <- ras_list_filtered[[i]]
    tile_name <- names(ras_list_filtered)[i] %||% paste0("Tile_", i)

    message("\n Processing ", tile_name)

    # ---------------------------------------------------------------------
    # STEP 3a: Process each time layer within the current tile
    # ---------------------------------------------------------------------
    per_year <- lapply(seq_len(terra::nlyr(tile)), function(k) {
      lyr <- tile[[k]]
      message("   ├─ Layer ", k, ": ", names(tile)[k])

      # Perform robust aggregation to PRIO scale
      # Note: tiled = TRUE ensures localized cropping of PRIO-GRID
      robust_transformation(lyr, block_fun, tiled = TRUE)
    })

    # ---------------------------------------------------------------------
    # STEP 3b: Stack valid layers back into a multi-layer raster
    # ---------------------------------------------------------------------
    # Layers that failed to process (NULL) are removed before stacking.
    # ---------------------------------------------------------------------
    per_year <- Filter(Negate(is.null), per_year)
    if (length(per_year) > 0) terra::rast(per_year) else NULL
  })


  # -------------------------------------------------------------------------
  # STEP 4: Merge processed tiles into a single raster
  # -------------------------------------------------------------------------
  # After all tiles have been processed independently, they are merged
  # spatially into a single continuous PRIO-aligned SpatRaster.
  # If only one tile exists, that raster is returned directly.
  # -------------------------------------------------------------------------
  results <- Filter(Negate(is.null), results)
  if (length(results) > 1) {
    message("\n Merging ", length(results), " aggregated tiles into final raster...")
    do.call(terra::merge, results)
  } else {
    results[[1]]
  }
}

#' Generate Cropland Raster Layers
#'
#' Aggregates rainfed, herbaceous, tree/orchard, and irrigated croplands (LC ids 10, 11, 12, 20)
#' into a unified cropland class. These represent actively cultivated areas used for agriculture,
#' distinguishing between different vegetation covers and irrigation practices.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated cropland raster.
gen_glc_cropland <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  glc_landcover(
    landcovertype = c(10, 11, 12, 20),
    beta_test = beta_test,
    foldersize = foldersize,
    n_tifs = n_tifs
  )
}

#' Generate Forest Raster Layers
#'
#' Aggregates all broadleaved, needle-leaved, and mixed-leaf forests (LC ids 51–92),
#' both open and closed canopy types. This grouping captures the range of forested
#' ecosystems from evergreen to deciduous forms, unified by dense perennial vegetation
#' cover exceeding typical shrub or grassland canopy thresholds.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated forest raster.
gen_glc_forest <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  glc_landcover(
    landcovertype = c(51, 52, 61, 62, 71, 72, 81, 82, 91, 92),
    beta_test = beta_test,
    foldersize = foldersize,
    n_tifs = n_tifs
  )
}

#' Generate Shrubland Raster Layers
#'
#' Combines evergreen, deciduous, and sparse shrublands (LC ids 120, 121, 122, 150, 152)
#' into a unified shrubland class. These classes represent areas dominated by woody
#' perennials shorter than trees, often found in semi-arid regions and ecotones between
#' grassland and forest ecosystems.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated shrubland raster.
gen_glc_shrubland <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  robust_glc_landcover(
    landcovertype = c(120, 121, 122, 150, 152),
    beta_test = beta_test,
    foldersize = foldersize,
    n_tifs = n_tifs
  )
}

#' Generate Grassland Raster Layers
#'
#' Merges open herbaceous and sparse grass-dominated areas (LC ids 130, 153)
#' into a single grassland class. These correspond to regions with continuous or
#' patchy coverage of grasses and herbaceous vegetation, typically maintained by
#' climate, fire, or grazing pressure.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated grassland raster.
gen_glc_grassland <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  glc_landcover(
    landcovertype = c(130, 153),
    beta_test = beta_test,
    foldersize = foldersize,
    n_tifs = n_tifs
  )
}

#' Generate Wetland Raster Layers
#'
#' Groups swamp, marsh, flooded, saline, mangrove, and tidal flats (LC ids 181–187)
#' into a unified wetland class. These represent permanently or seasonally waterlogged
#' ecosystems characterized by saturated soils, distinct hydrology, and vegetation adapted
#' to anaerobic conditions.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated wetland raster.
gen_glc_wetland <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  glc_landcover(
    landcovertype = c(181, 182, 183, 184, 185, 186, 187),
    beta_test = beta_test,
    foldersize = foldersize,
    n_tifs = n_tifs
  )
}

#' Generate Built-up / Urban Raster Layers
#'
#' Captures impervious or constructed surfaces (LC id 190) corresponding to built-up or
#' urban areas. These include settlements, infrastructure, and other human-made structures
#' with minimal vegetative cover.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated built-up raster.
gen_glc_builtup <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  glc_landcover(
    landcovertype = 190,
    beta_test = beta_test
  )
}

#' Generate Water Body Raster Layers
#'
#' Represents inland and coastal water bodies (LC id 210), including lakes, reservoirs,
#' and rivers. This grouping excludes wetlands (181–187), focusing solely on standing or
#' flowing open water.
#'
#' @inheritParams prepare_glc_layers
#' @return Aggregated water body raster.
gen_glc_water <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  glc_landcover(
    landcovertype = 210,
    beta_test = beta_test
  )
}

