process_all_tiles_summary <- function(all_tiles, pg_sf) {
  library(dplyr)
  library(tidyr)
  library(terra)
  library(sf)

  full_summary_df <- data.frame()  # Initialize master summary table

  for (tile_id in all_tiles) {
    cat("🔄 Processing tile:", tile_id, "\n")

    # Start timer
    start_time <- Sys.time()

    # Step 1: Load rasters
    tile_data <- load_and_plot_tile_pair(tile_id, plot = FALSE)
    if (is.null(tile_data$r_2019) || is.null(tile_data$r_evo)) {
      cat("⚠️ Skipping tile:", tile_id, "due to missing data\n")
      next
    }

    # Step 2: Process rasters
    processed <- process_tile_pair(
      r_2019 = tile_data$r_2019,
      r_evo  = tile_data$r_evo
    )

    # Step 3: Combine layers
    r_combined <- cover(processed$r_evo_upsampled, processed$r_2019_newest)

    # Step 4: Filter PG cells within raster extent
    pg_subset <- filter_pg_cells_within_raster(pg_sf, r_combined)

    # Step 5: Summarize built-up area by PG cell
    tile_summary <- summarize_built_up_by_pg_cell(r_combined, pg_subset)

    # Step 6: Add tile ID column and reposition
    tile_summary <- tile_summary %>%
      mutate(tile_id = tile_id) %>%
      relocate(tile_id, .after = pg_id)

    # Step 7: Append to main results
    full_summary_df <- bind_rows(full_summary_df, tile_summary)

    # End timer
    end_time <- Sys.time()
    duration_min <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

    # Clean up
    rm(tile_data, processed, r_combined, pg_subset, tile_summary)
    gc()

    cat("✅ Finished tile:", tile_id, "in", duration_min, "minutes\n\n")
  }

  return(full_summary_df)
}
