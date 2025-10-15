#acknowledged no docstrings added yet.

#------------------------------------------------------------------------------
# FUNCTION 1:
#------------------------------------------------------------------------------

read_glc_v2 <- function(beta_test = FALSE, foldersize = NULL) {
  # Base dataset directory
  f <- file.path(
    "/Volumes/T7/PRIOGRID",
    "GLC_FCS30",
    "v2",
    "7f03a296-4329-4458-8b62-83c3d27530af"
  )

  # ---- Identify zip files ----
  zips <- list.files(f, pattern = "\\.zip$", full.names = TRUE)

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

prepare_glc_layers <- function(beta_test = FALSE, foldersize = NULL) {
  library(terra)
  library(lubridate)

  # ---- Step 1: Read TIFF files from GLC folders ----
  tif_files <- read_glc_v2(beta_test = beta_test, foldersize = foldersize)

  message("Found ", length(tif_files), " TIFF files to process.\n")

  # ---- Step 1: Load and rename rasters by year range ----
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

robust_glc_landcover <- function(landcovertype, beta_test = FALSE, foldersize = NULL) {
  memfrac_option <- terra::terraOptions(verbose = FALSE)$memfrac
  terra::terraOptions(memfrac = 0.8)

  # Read and prepare all tiles
  ras_list_filtered <- prepare_glc_layers(beta_test = beta_test, foldersize = foldersize)


  message("Running landcover computation on ", length(ras_list_filtered), " tiles.")
  message("Each tile will be processed via robust_transformation.\n")

  # Apply robust transformation to each tile
  transformed_tiles <- lapply(seq_along(ras_list_filtered), function(i) {
    tile <- ras_list_filtered[[i]]
    tile_name <- names(ras_list_filtered)[i] %||% paste0("Tile_", i)

    message(i, "/", length(ras_list_filtered), "] Processing ", tile_name)

    tryCatch({
      res_tile <- robust_transformation(
        tile,
        function(x) mean(x %in% landcovertype, na.rm = TRUE)
      )
      gc()
      res_tile
    }, error = function(e) {
      message("Skipping problematic tile: ", tile_name, " — ", e$message)
      NULL
    })
  })

  # Drop NULLs
  transformed_tiles <- Filter(Negate(is.null), transformed_tiles)

  # Merge results
  if (length(transformed_tiles) > 1) {
    message("\n Merging ", length(transformed_tiles), " aggregated tiles...")
    res <- do.call(terra::merge, transformed_tiles)
  } else {
    res <- transformed_tiles[[1]]
  }

  message("Landcover computation complete.")
  terra::terraOptions(memfrac = memfrac_option)
  return(res)
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
gen_glc_shrubland <- function(beta_test = FALSE, foldersize = NULL) {
  glc_landcover(
    landcovertype = c(120, 121, 122, 150, 152),
    beta_test = beta_test,
    foldersize = foldersize
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
