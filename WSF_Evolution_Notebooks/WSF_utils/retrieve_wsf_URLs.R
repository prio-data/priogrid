summarize_wsf_tile_coverage <- function() {
  library(rvest)
  library(stringr)
  library(readr)

  # Evolution
  evo_base_url <- "https://download.geoservice.dlr.de/WSF_EVO/files/"
  evo_page <- read_html(evo_base_url)
  evo_all_links <- evo_page %>% html_nodes("a") %>% html_attr("href")
  evo_tif_links <- evo_all_links[grepl("WSFevolution_v1_.*\\.tif$", evo_all_links)]
  evo_full_urls <- paste0(evo_base_url, evo_tif_links)
  evo_coord_tiles <- sub(".*_v1_(.*)\\.tif$", "\\1", evo_full_urls)

  # Save
  write_csv(data.frame(tif_url = evo_full_urls), "wsf_evo_tile_list.csv")

  # WSF 2019
  wsf2019_base_url <- "https://download.geoservice.dlr.de/WSF2019/files/"
  wsf2019_page <- read_html(wsf2019_base_url)
  wsf2019_all_links <- wsf2019_page %>% html_nodes("a") %>% html_attr("href")
  wsf2019_tif_links <- wsf2019_all_links[grepl("WSF2019_v1_.*\\.tif$", wsf2019_all_links)]
  wsf2019_full_urls <- paste0(wsf2019_base_url, wsf2019_tif_links)
  wsf2019_coord_tiles <- sub(".*_v1_(.*)\\.tif$", "\\1", wsf2019_full_urls)

  # Save
  write_csv(data.frame(tif_url = wsf2019_full_urls), "wsf_2019_tile_list.csv")

  # Compare
  common_tiles <- intersect(wsf2019_coord_tiles, evo_coord_tiles)
  only_in_2019 <- setdiff(wsf2019_coord_tiles, evo_coord_tiles)
  only_in_evo <- setdiff(evo_coord_tiles, wsf2019_coord_tiles)

  # Summary
  cat("🗂️  TILE COVERAGE SUMMARY\n")
  cat("Total WSF Evolution tiles: ", length(evo_coord_tiles), "\n")
  cat("Total WSF 2019 tiles:      ", length(wsf2019_coord_tiles), "\n")
  cat("Shared tiles:              ", length(common_tiles), "\n")
  cat("Only in WSF 2019:          ", length(only_in_2019), "\n")
  cat("Only in WSF Evolution:     ", length(only_in_evo), "\n")

  # Return useful objects for downstream functions
  return(list(
    evo_urls = evo_full_urls,
    wsf2019_urls = wsf2019_full_urls,
    common_tile_ids = common_tiles
  ))
}

load_and_plot_tile_pair <- function(tile_id, plot = TRUE) {
  library(terra)

  # Construct URLs for the selected tile
  evo_url <- paste0("https://download.geoservice.dlr.de/WSF_EVO/files/WSFevolution_v1_", tile_id, ".tif")
  wsf2019_url <- paste0("https://download.geoservice.dlr.de/WSF2019/files/WSF2019_v1_", tile_id, ".tif")

  # Create temporary file paths
  evo_temp <- tempfile(fileext = ".tif")
  wsf2019_temp <- tempfile(fileext = ".tif")

  # Download the GeoTIFFs
  download.file(evo_url, destfile = evo_temp, mode = "wb", quiet = TRUE)
  download.file(wsf2019_url, destfile = wsf2019_temp, mode = "wb", quiet = TRUE)

  # Load as terra rasters
  r_evo <- rast(evo_temp)
  r_2019 <- rast(wsf2019_temp)

  # Optional side-by-side plot
  if (plot) {
    par(mfrow = c(1, 2))
    plot(r_2019, main = paste("WSF 2019 -", tile_id))
    plot(r_evo, main = paste("WSF Evolution -", tile_id))
    par(mfrow = c(1, 1))
  }

  # Return both rasters as a list
  return(list(
    tile_id = tile_id,
    r_2019 = r_2019,
    r_evo = r_evo
  ))
}

