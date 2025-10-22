#acknowledged no docstrings added yet.

#------------------------------------------------------------------------------
# FUNCTION 1:
#------------------------------------------------------------------------------

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

  dir <- dirname(Reduce(function(x, y) {
    while (!startsWith(y, x)) x <- dirname(x)
    x
  }, f))

  # ---- Identify zip files ----
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
#' Aggregates high-resolution Global Landcover (GLC) tiles into PRIO-GRID scale rasters.
#' Each 5×5° tile is processed independently, layer-by-layer, using a robust and memory-safe
#' transformation that automatically aligns PRIO-GRID extents with the tile extent.
#'
#' This function is optimized for distributed or parallel workflows and is resilient
#' to small tile extent inconsistencies due to the `tiled = TRUE` flag in robust_transformation().
#'
#' @param landcovertype Integer or vector of integer GLC codes representing the landcover
#'   classes of interest (e.g., cropland = c(10, 11, 12)).
#' @param beta_test Logical; if TRUE, runs in test mode (typically loads a limited number of tiles).
#' @param foldersize Optional integer controlling how many tiles to process in one batch.
#' @param n_tifs Optional integer limiting the number of raster files to process (for testing).
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

    message("\n🧩 Processing ", tile_name)

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
    message("\n🌍 Merging ", length(results), " aggregated tiles into final raster...")
    do.call(terra::merge, results)
  } else {
    results[[1]]
  }
}



# --- Cropland ---
gen_glc_cropland <- function(beta_test = FALSE, foldersize = NULL) {
  glc_landcover(
    landcovertype = c(10, 11, 12, 20),
    beta_test = beta_test,
    foldersize = foldersize
  )
}

# --- Forest ---
gen_glc_forest <- function(beta_test = FALSE) {
  glc_landcover(
    landcovertype = c(51, 52, 61, 62, 71, 72, 81, 82, 91, 92),
    beta_test = beta_test
  )
}

# --- Shrubland ---
gen_glc_shrubland <- function(beta_test = FALSE, foldersize = NULL, n_tifs = NULL) {
  robust_glc_landcover(
    landcovertype = c(120, 121, 122, 150, 152),
    beta_test = beta_test,
    foldersize = foldersize,
    n_tifs = n_tifs
  )
}

# --- Grassland ---
gen_glc_grassland <- function(beta_test = FALSE) {
  glc_landcover(
    landcovertype = c(130, 153),
    beta_test = beta_test
  )
}

# --- Wetland ---
gen_glc_wetland <- function(beta_test = FALSE) {
  glc_landcover(
    landcovertype = c(181, 182, 183, 184, 185, 186, 187),
    beta_test = beta_test
  )
}

# --- Built-up / Urban ---
gen_glc_builtup <- function(beta_test = FALSE) {
  glc_landcover(
    landcovertype = 190,
    beta_test = beta_test
  )
}

# --- Water Body ---
gen_glc_water <- function(beta_test = FALSE) {
  glc_landcover(
    landcovertype = 210,
    beta_test = beta_test
  )
}
