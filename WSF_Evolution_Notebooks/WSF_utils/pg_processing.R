library(terra)
library(sf)
library(dplyr)
library(tidyr)

filter_pg_cells_within_raster <- function(pg_sf, raster_layer) {

  # Step 1: Transform PG grid to raster CRS
  pg_sf_aligned <- st_transform(pg_sf, crs = crs(raster_layer))

  # Step 2: Convert raster extent to an sf polygon
  r_extent_poly <- st_as_sfc(st_bbox(raster_layer))
  st_crs(r_extent_poly) <- st_crs(pg_sf_aligned)  # Ensure CRS match

  # Step 3: Keep only polygons fully within raster extent
  pg_in_raster <- pg_sf_aligned[st_within(pg_sf_aligned, r_extent_poly, sparse = FALSE), ]

  return(pg_in_raster)
}


summarize_built_up_by_pg_cell <- function(r_combined, pg) {

  # Year bins and labels
  year_bins <- c(1984, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
  bin_labels <- c("1985", "1986-1990", "1991-1995", "1996-2000",
                  "2001-2005", "2006-2010", "2011-2015", "2016-2019")

  # Reproject raster and polygons to a projected CRS (for accurate area)
  target_crs <- "EPSG:3857"
  r_proj <- project(r_combined, target_crs)
  pg_proj <- st_transform(pg, crs = target_crs)

  result_list <- vector("list", length = nrow(pg_proj))

  for (i in seq_len(nrow(pg_proj))) {
    pg_cell <- pg_proj[i, ]
    pg_id <- pg_cell$pg_id
    pg_vect <- vect(pg_cell)
    pg_area_m2 <- expanse(pg_vect, unit = "m")

    r_crop <- crop(r_proj, pg_vect)
    r_masked <- mask(r_crop, pg_vect)

    vals <- values(r_masked)
    vals <- vals[!is.na(vals)]

    if (length(vals) == 0) next

    # Bin raster values into year intervals
    year_bin <- cut(vals, breaks = year_bins, labels = bin_labels, right = TRUE)
    bin_table <- table(year_bin)

    # Compute pixel area
    res_m <- res(r_proj)
    px_area <- res_m[1] * res_m[2]

    # Total built-up area
    built_area_m2 <- length(vals) * px_area

    # Proportional built-up per bin
    row <- as.list(setNames(rep(0, length(bin_labels)), bin_labels))
    bin_area_vals <- as.numeric(bin_table) * px_area
    names(bin_area_vals) <- names(bin_table)
    row[names(bin_area_vals)] <- bin_area_vals / pg_area_m2

    # Add identifiers and metrics
    row$pg_id <- pg_id
    row$prop_built_up <- built_area_m2 / pg_area_m2
    row$total_bua <- built_area_m2  # NEW FIELD: total built-up area in m²

    result_list[[i]] <- row
  }

  final_df <- bind_rows(result_list) %>%
    replace(is.na(.), 0) %>%
    select(pg_id, prop_built_up, total_bua, all_of(bin_labels))

  return(final_df)
}


