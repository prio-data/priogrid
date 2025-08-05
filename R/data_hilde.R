#' Reads the HILDE+ data
#'
#'
#'
#' @return an object of class sf
#' @export
#'
#' @references
#' \insertRef{winklerHILDAGlobalLand2020}{priogrid}
read_hilde <- function() {
  f <- get_pgfile(source_name = "HILDE",
                  source_version = "v1.0",
                  id = "82bc4c6f-9904-484f-aa9a-77771d076690")

  # Unzip archive
  unzipped_directory <- zip_file(f)

  # Construct the path to the raster state files directory
  states_dir <- file.path(unzipped_directory,
                          "hildap_vGLOB-1.0_geotiff_wgs84",
                          "hildap_GLOB-v1.0_lulc-states")

  # Filter raster .tif files to those matching PRIO-GRID year intervals
  filtered_files <- filter_tifs_by_pg_intervals(states_dir)

  r <- terra::rast(filtered_files)

  return(r)
}

#' Filter File Paths by Year
#'
#' Filters a list of file paths by extracting the 4-digit year from each file name
#' and retaining only those within the specified year range.
#'
#' @param file_paths Character vector. A list of file paths with year-encoded filenames.
#' @param start_year Integer. The beginning year of the desired range (inclusive).
#' @param end_year Integer. The ending year of the desired range (inclusive).
#'
#' @return A character vector of file paths that fall within the specified year range.
#'
#' @examples
#' files <- c("lulc_state_1992.tif", "lulc_state_2001.tif", "lulc_state_2015.tif")
#' filtered <- filter_files_by_year(files, 2000, 2020)
#' print(filtered)
#'
filter_files_by_year <- function(file_paths, start_year, end_year) {
  # Extract 4-digit year from each file name
  years <- as.numeric(sub(".*?(\\d{4}).*", "\\1", basename(file_paths)))

  # Filter files based on year range
  keep <- years >= start_year & years <= end_year
  return(file_paths[keep])
}

#-----

#' Filter Raster Files Based on PRIO-GRID Interval Years
#'
#' Filters a list of raster `.tif` files to include only those whose filenames
#' contain years matching the PRIO-GRID temporal extent returned by `pg_date_intervals()`.
#'
#' @param states_dir Character. File path to the directory containing `.tif` raster files.
#'
#' @return A character vector of filtered `.tif` file paths whose names include years within the PRIO-GRID interval.
#'
#' @examples
#' filtered_files <- filter_tifs_by_pg_intervals("/path/to/your/hilda/states")
#'
#' @importFrom stringr str_extract
#' @export
filter_tifs_by_pg_intervals <- function(states_dir) {
  # Get PRIO-GRID date intervals
  pg_intervals <- pg_date_intervals()

  # Extract start and end years from intervals
  start_year <- as.integer(stringr::str_extract(pg_intervals[1], "^\\d{4}"))
  end_year <- as.integer(stringr::str_extract(pg_intervals[length(pg_intervals)], "^\\d{4}"))

  # List all .tif files in the specified directory
  tif_files <- list.files(states_dir, pattern = "\\.tif$", full.names = TRUE)

  # Filter files based on the extracted year range
  filtered_files <- filter_files_by_year(tif_files, start_year, end_year)

  return(filtered_files)
}

#-----


#' Get Valid Tile Sizes for PRIO-GRID Extent
#'
#' Computes a list of valid tile sizes that evenly divide both the longitudinal and latitudinal
#' extents of the input `pg_grid` raster.
#'
#' @param pg_grid A SpatRaster object representing the PRIO-GRID raster.
#' @param increment Integer. Step size (in degrees) to evaluate for tiling (default is 10).
#'
#' @return A numeric vector of valid tile sizes (in degrees) that evenly divide the grid extent.
#'
#' @examples
#' valid_sizes <- get_valid_tile_sizes(pg_grid)
#'
#' @export
get_valid_tile_sizes <- function(pg_grid, increment = 10) {
  e <- ext(pg_grid)

  x_extent <- e[2] - e[1]  # xmax - xmin
  y_extent <- e[4] - e[3]  # ymax - ymin

  max_size <- min(x_extent, y_extent)
  candidates <- seq(increment, max_size, by = increment)

  valid_sizes <- candidates[
    (x_extent %% candidates) == 0 & (y_extent %% candidates) == 0
  ]

  return(valid_sizes)
}

#' Generate Tiling Grid Over PRIO-GRID Extent
#'
#' Generates a tiling grid over the PRIO-GRID raster extent, ensuring the specified `tile_size`
#' evenly divides the full spatial extent. Raises an error if the input tile size is invalid.
#'
#' @param pg_grid A SpatRaster object representing the PRIO-GRID raster.
#' @param tile_size Integer. Desired size of the tile (in degrees).
#' @param increment Integer. Step size to evaluate valid tile sizes (default is 10).
#'
#' @return A data.frame with one row per tile, including the bounding box coordinates and a tile ID.
#'
#' @examples
#' tiles <- generate_tiles_checked(pg_grid, tile_size = 10)
#'
#' @export

generate_tiles_checked <- function(pg_grid, tile_size, increment = 10) {
  # Step 1: Get valid sizes based on pg_grid extent
  valid_sizes <- get_valid_tile_sizes(pg_grid, increment = increment)

  # Step 2: Validate user input
  if (!(tile_size %in% valid_sizes)) {
    stop(sprintf(
      "Invalid tile size: %d°. It does not divide evenly into the PRIO-GRID extent.\n Valid options: %s",
      tile_size,
      paste(valid_sizes, collapse = ", ")
    ))
  }

  # Step 3: Extract extent and generate tile boundaries
  e <- ext(pg_grid)
  xmin <- floor(e[1])
  xmax <- ceiling(e[2])
  ymin <- floor(e[3])
  ymax <- ceiling(e[4])

  lon_seq <- seq(xmin, xmax - tile_size, by = tile_size)
  lat_seq <- seq(ymin, ymax - tile_size, by = tile_size)

  grid <- expand.grid(lon = lon_seq, lat = lat_seq)
  tile_df <- data.frame(
    tile_id = seq_len(nrow(grid)),
    xmin = grid$lon,
    xmax = grid$lon + tile_size,
    ymin = grid$lat,
    ymax = grid$lat + tile_size
  )

  return(tile_df)
}

#-----


#' Process Land Cover Data by PRIO-GRID Tiles with Area Proportions
#'
#' Processes a single land cover raster file by dividing the global extent into tiles and
#' extracting the proportional area of each land cover class per PRIO-GRID cell.
#' This approach conserves memory and improves processing speed by working in spatial chunks.
#'
#' @param tif_path Character. File path to a single land cover raster (.tif) dataset.
#' @param pg_vect SpatVector. Vectorized PRIO-GRID polygons with a 'pg_id' attribute.
#' @param tile_df Data frame. Output from `generate_tiles_checked()` containing tile bounding boxes.
#' @param runtest Logical. If TRUE, only a small subset of tiles (28:30) is processed for test purposes. Default is FALSE.
#'
#' @return A data frame with proportional land cover area per `pg_id`, `landcover_type`, and `year`.
#' Includes fields:
#' \itemize{
#'   \item `pg_id`: Unique PRIO-GRID identifier
#'   \item `landcover_type`: Integer land cover class from raster
#'   \item `year`: Year parsed from the raster filename
#'   \item `p_area`: Proportion of grid cell covered by this land cover type
#'   \item `p_area_sum`: Sum of all land cover proportions within the grid cell (should be ~1)
#'   \item `fully_covered`: Logical indicating if cell coverage is complete
#' }
#'
#' @examples
#' result <- process_landcover_in_tiles_proportional("hilda_plus_2000_states_GLOB-v1-0_wgs84-nn.tif", pg_vect, tile_df)
#'
#' @importFrom terra rast crop cellSize extract ext values ncell expanse
#' @importFrom dplyr group_by summarise mutate filter select bind_rows left_join
#' @export

process_landcover_in_tiles_proportional <- function(tif_path, pg_vect, tile_df, runtest = FALSE) {
  r <- rast(tif_path)
  year <- as.numeric(sub(".*?(\\d{4}).*", "\\1", basename(tif_path)))

  results <- list()
  total_start <- Sys.time()

  # Optional test subset
  if (runtest == TRUE) {
    tile_df <- tile_df[28:29, ]
  }

  # Create bounding box for selected tiles
  tile_extent <- ext(
    min(tile_df$xmin), max(tile_df$xmax),
    min(tile_df$ymin), max(tile_df$ymax)
  )

  # Crop vector to relevant region
  pg_vect <- crop(pg_vect, tile_extent)

  # Precompute total cell area (in km²)
  message("Calculating total area of PRIO-GRID polygons (pg_id)...")
  pg_vect$cell_area_km2 <- expanse(pg_vect, unit = "km", transform = TRUE)

  for (i in seq_len(nrow(tile_df))) {
    tile_start <- Sys.time()

    tile_info <- tile_df[i, ]
    message(sprintf("Processing tile %d of %d: [%d°, %d°, %d°, %d°]",
                    i, nrow(tile_df),
                    tile_info$xmin, tile_info$xmax,
                    tile_info$ymin, tile_info$ymax))

    # Crop raster to tile
    bbox <- ext(tile_info$xmin, tile_info$xmax, tile_info$ymin, tile_info$ymax)
    r_tile <- crop(r, bbox)

    if (ncell(r_tile) == 0 || all(is.na(values(r_tile)))) {
      message(sprintf("Skipping tile %d (no data)", i))
      next
    }

    # Compute accurate per-pixel area
    area_tile <- cellSize(r_tile, unit = "km")
    vals <- c(r_tile, area_tile)
    names(vals) <- c("landcover_type", "area_km2")

    # Extract landcover and pixel area per pg_id
    extract_df <- terra::extract(vals, pg_vect, bind = FALSE)

    if (nrow(extract_df) == 0) next

    # Add pg_id
    extract_df$pg_id <- pg_vect$pg_id[extract_df$ID]
    extract_df$cell_area_km2 <- pg_vect$cell_area_km2[extract_df$ID]

    # Summarize area per landcover type and pg_id
    long_df <- extract_df %>%
      dplyr::filter(!is.na(landcover_type)) %>%
      dplyr::group_by(pg_id, landcover_type) %>%
      dplyr::summarise(
        area_km2 = sum(area_km2, na.rm = TRUE),
        cell_area_km2 = dplyr::first(cell_area_km2),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        landcover_type = as.integer(landcover_type),
        year = year,
        p_area = area_km2 / cell_area_km2
      ) %>%
      dplyr::select(pg_id, landcover_type, year, area_km2, cell_area_km2)


    results[[i]] <- long_df

    tile_time <- difftime(Sys.time(), tile_start, units = "mins")
    message(sprintf("Finished tile %d in %.2f minutes", i, tile_time))
  }

  # Combine and summarize per landcover type
  combined_df <- bind_rows(results) %>%
    group_by(pg_id, landcover_type, year) %>%
    summarise(
      area_km2 = sum(area_km2, na.rm = TRUE),
      cell_area_km2 = first(cell_area_km2),  # constant per pg_id
      .groups = "drop"
    ) %>%
    mutate(
      p_area = area_km2 / cell_area_km2
    )

  # Optional: check coverage completeness by summing p_area per pg_id
  coverage_check <- combined_df %>%
    group_by(pg_id, year) %>%
    summarise(
      p_area_sum = sum(p_area, na.rm = TRUE),
      fully_covered = abs(sum(p_area, na.rm = TRUE) - 1) < 1e-4,
      .groups = "drop"
    )

  # Optionally join this check back to the main results
  combined_df <- combined_df %>%
    dplyr::left_join(coverage_check, by = c("pg_id", "year")) %>%
    dplyr::select(pg_id, landcover_type, year, p_area, p_area_sum, fully_covered)


  # Runtime message
  total_time <- difftime(Sys.time(), total_start, units = "mins")
  message(sprintf("Finished all tiles for year %d in %.2f minutes", year, total_time))

  return(combined_df)
}

#-----

#' Create Raster Stack of Land Cover Proportions per PRIO-GRID Cell
#'
#' Converts a data frame of land cover proportions (output from
#' `process_landcover_in_tiles_proportional()`) into a raster stack. Each layer represents
#' the proportional area of a specific land cover type in each PRIO-GRID cell.
#'
#' @param lc_output Data frame. Output from `process_landcover_in_tiles_proportional()`, containing columns:
#'   - `pg_id`: PRIO-GRID cell ID
#'   - `landcover_type`: Integer representing the land cover class
#'   - `p_area`: Proportion of the grid cell area covered by the class
#' @param pg_grid SpatRaster. PRIO-GRID raster template, where each cell value represents a `pg_id`.
#'
#' @return A `SpatRaster` stack with one layer per land cover type, plus one base layer (`pg_id`).
#'   Each layer name follows the format `"lc_<type>"`.
#'
#' @examples
#' r_stack <- create_landcover_area_stack(lc_output_test, pg_grid)
#' plot(r_stack)
#'
#' @importFrom dplyr mutate filter
#' @importFrom terra classify
#' @export

create_landcover_area_stack <- function(lc_output, pg_grid) {
  # Ensure pg_id is integer to match raster values
  lc_output <- lc_output %>%
    mutate(pg_id = as.integer(pg_id))

  # Get unique landcover types
  landcover_types <- sort(unique(lc_output$landcover_type))

  # Initialize list to hold area rasters
  area_rasters <- list()

  # Loop through each landcover type and classify
  for (lc in landcover_types) {
    lc_subset <- lc_output %>% filter(landcover_type == lc)
    lookup <- lc_subset[, c("pg_id", "p_area")]

    area_r <- classify(pg_grid, lookup, others = NA)
    names(area_r) <- paste0("lc_", lc)

    area_rasters[[length(area_rasters) + 1]] <- area_r
  }

  # Stack: first layer is pg_id, rest are landcover area rasters
  r_stack <- c(pg_grid, do.call(c, area_rasters))
  names(r_stack)[1] <- "pg_id"

  return(r_stack)
}

#-----

#' Batch Process Land Cover Rasters by PRIO-GRID Tiles with Area Proportions
#'
#' Applies the `process_landcover_in_tiles_proportional()` function across a list of
#' raster files, each representing a unique year of land cover data. This function
#' compiles the proportional land cover area per PRIO-GRID cell into a single data frame.
#'
#' @param file_list Character vector. A list of `.tif` file paths, each corresponding to a unique year.
#' @param pg_vect SpatVector. Vectorized PRIO-GRID polygons with a `pg_id` attribute.
#' @param tile_df Data frame. Output from `generate_tiles_checked()`, containing tile bounding boxes.
#' @param runtest Logical. If TRUE, only a small subset of tiles is processed for debugging. Default is FALSE.
#'
#' @return A data frame combining proportional land cover data (`p_area`) across all years and PRIO-GRID cells.
#' Contains the following columns:
#' \itemize{
#'   \item `pg_id`: Unique PRIO-GRID identifier
#'   \item `landcover_type`: Integer land cover class from raster
#'   \item `year`: Year parsed from raster filename
#'   \item `p_area`: Proportion of grid cell covered by this land cover type
#'   \item `p_area_sum`: Sum of all land cover proportions within the grid cell (should be ~1)
#'   \item `fully_covered`: Logical indicating if cell coverage is complete
#' }
#'
#' @examples
#' \dontrun{
#' all_years_df <- batch_process_landcover_in_tiles_proportional(filtered_files, pg_vect, tile_df)
#' }
#'
#' @export
batch_process_landcover_in_tiles_proportional <- function(file_list, pg_vect, tile_df, runtest = FALSE) {
  # Initialize an empty data frame to store results from all years
  year_compile_df <- data.frame(
    pg_id = integer(),
    landcover_type = integer(),
    year = integer(),
    p_area = numeric(),
    p_area_sum = numeric(),
    fully_covered = logical(),
    stringsAsFactors = FALSE
  )

  # Loop over all files (each representing a unique year)
  for (i in file_list) {
    message("Processing file: ", i)

    # Process the current raster file
    result <- process_landcover_in_tiles_proportional(
      tif_path = i,
      pg_vect = pg_vect,
      tile_df = tile_df,
      runtest = runtest
    )

    # Append to the cumulative results
    year_compile_df <- dplyr::bind_rows(year_compile_df, result)

    message("Finished file: ", i, "\n")
  }

  return(year_compile_df)
}

#-----

#' Batch Create Raster Stacks of Land Cover Proportions by Year
#'
#' Converts a multi-year data frame of proportional land cover values (e.g., from
#' `batch_process_landcover_in_tiles_proportional()`) into a named list of
#' `SpatRaster` stacks—one per unique year. Each stack contains raster layers
#' representing the proportion of PRIO-GRID cell area covered by each land cover type.
#'
#' @param year_compile_df Data frame. Must include `pg_id`, `landcover_type`, `p_area`, and `year`.
#' @param pg_grid SpatRaster. Template raster where each cell is assigned a `pg_id`.
#'
#' @return A named list of `SpatRaster` stacks. Each element corresponds to a year and contains:
#' \itemize{
#'   \item Layer 1: `pg_id` values
#'   \item Layers 2+: Proportional rasters for each land cover type (e.g., `lc_10`, `lc_22`, etc.)
#' }
#'
#' @examples
#' \dontrun{
#' landcover_stack_list <- batch_create_landcover_area_stack(year_compile_df, pg_grid)
#' plot(landcover_stack_list[["2005"]][["lc_22"]])
#' }
#'
#' @importFrom dplyr filter
#' @export
batch_create_landcover_area_stack <- function(year_compile_df, pg_grid) {
  # Identify unique years
  unique_years <- sort(unique(year_compile_df$year))

  # Initialize a named list to store raster stacks per year
  landcover_stack_list <- list()

  # Loop through each year and create a raster stack
  for (yr in unique_years) {
    message("Processing raster stack for year: ", yr)

    # Filter to one year
    lc_year_df <- dplyr::filter(year_compile_df, year == yr)

    # Create raster stack for that year
    r_stack <- create_landcover_area_stack(lc_year_df, pg_grid)

    # Store in list with year as name
    landcover_stack_list[[as.character(yr)]] <- r_stack
  }

  return(landcover_stack_list)
}

#-----

#' Generate Annual Raster Stacks of Land Cover Proportions from HILDA+ Archive
#'
#' Unzips the HILDA+ raster archive, filters land cover state `.tif` files based on
#' PRIO-GRID-supported years, and processes them to compute the proportional area of
#' each land cover class per PRIO-GRID cell. Final output is a named list of annual
#' raster stacks where each layer represents a unique land cover type.
#'
#' @param zip_path Optional character. Path to the zipped HILDA+ archive. If NULL, `read_hilde()` is called internally.
#' @param tile_size Numeric. Size of the tiling unit in degrees for spatial partitioning (default = 30).
#' @param runtest Logical. If TRUE, only a subset of tiles are processed for testing (default = FALSE).
#'
#' @return A named list of `SpatRaster` stacks. Each list element corresponds to a year and contains:
#' \itemize{
#'   \item Layer 1: `pg_id` values (PRIO-GRID cell IDs)
#'   \item Layers 2+: Proportional rasters for each land cover type (`lc_10`, `lc_22`, etc.)
#' }
#'
#' @examples
#' \dontrun{
#' lc_stacks <- generate_landcover_stack_list()
#' plot(lc_stacks[["2005"]][["lc_22"]])
#' }
#'
#' @importFrom terra rast crop classify cellSize expanse extract as.polygons ext values ncell
#' @importFrom dplyr bind_rows filter mutate group_by summarise select left_join
#' @importFrom stringr str_extract
#' @export
generate_landcover_stack_list <- function(zip_path = NULL, tile_size = 30, runtest = FALSE) {
  # If no path supplied, read from the default HILDA+ fetcher
  if (is.null(zip_path)) {
    zip_path <- read_hilde()
  }

  # Unzip archive
  unzipped_directory <- zip_file(zip_path)

  # Construct the path to the raster state files directory
  states_dir <- file.path(unzipped_directory,
                          "hildap_vGLOB-1.0_geotiff_wgs84",
                          "hildap_GLOB-v1.0_lulc-states")

  # Filter raster .tif files to those matching PRIO-GRID year intervals
  filtered_files <- filter_tifs_by_pg_intervals(states_dir)

  # Generate PRIO-GRID raster and convert to vector
  pg_grid <- prio_blank_grid()
  pg_grid[] <- as.integer(round(pg_grid[]))
  names(pg_grid) <- "pg_id"
  pg_vect <- as.polygons(pg_grid, na.rm = TRUE)
  names(pg_vect) <- "pg_id"

  # Generate tile definitions
  tile_df <- generate_tiles_checked(pg_grid, tile_size = tile_size)

  # Process each file (year) into proportional land cover summaries
  year_compile_df <- batch_process_landcover_in_tiles_proportional(
    filtered_files, pg_vect, tile_df, runtest = runtest
  )

  # Build annual raster stacks from combined output
  landcover_stack_list <- batch_create_landcover_area_stack(year_compile_df, pg_grid)

  return(landcover_stack_list)
}


hilde_landcover <- function(landcovertype, max_cells_in_memory = (18000*36000*2)){
  r <- read_hilde()
  my_fun <- function(values, coverage_fractions){weighted.mean(values == landcovertype, coverage_fractions)}
  pg <- prio_blank_grid()
  pg_vect <- terra::as.polygons(pg) |> sf::st_as_sf()

  # Check if projection of r is same as pg, and transform accordingly
  # Check if extent of r is same as pg, and crop accordingly

  res <- exactextractr::exact_extract(r, pg_vect, fun = my_fun, max_cells_in_memory = max_cells_in_memory, stack_apply = T)
  all <- lapply(1:ncol(res), function(i){
    values(pg) <- res[,i]
    pg <- terra::flip(pg)
    pg
  })
  all <- terra::rast(all)

  # Make sure that layer names are reflecting the dates of the rasters.
  # names(all) <- ...

  all
}

# Reference: https://ceos.org/gst/HILDAplus.html
#
#
# OCEAN
gen_hilde_ocean <- function(){
  hilde_landcover(landcovertype == 00) # assuming 66 is the barren land type
}

# URBAN
gen_hilde_urban <- function(){
  hilde_landcover(landcovertype == 11) # assuming 66 is the barren land type
}

# CROPLAND
gen_hilde_cropland <- function(){
  hilde_landcover(landcovertype == 22) # assuming 66 is the barren land type
}

# PASTURE/RANGELAND
gen_hilde_pasture_rangeland <- function(){
  hilde_landcover(landcovertype == 33) # assuming 66 is the barren land type
}
# FOREST
gen_hilde_forest <- function(){
  hilde_landcover(landcovertype == 44) # assuming 66 is the barren land type
}

# UNAMANGED GRASS/SHRUBLAND
gen_hilde_unmanaged_grass_shrubland <- function(){
  hilde_landcover(landcovertype == 55) # assuming 66 is the barren land type
}

# SPARSE/ NO VEGETATION
gen_hilde_sparse_no_vegetation <- function(){
  hilde_landcover(landcovertype == 66) # assuming 66 is the barren land type
}

# WATER
gen_hilde_water <- function(){
  hilde_landcover(landcovertype == 77) # assuming 66 is the barren land type
}

# NO DATA
gen_hilde_nodata <- function(){
  hilde_landcover(landcovertype == 99) # assuming 66 is the barren land type
}
