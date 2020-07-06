#' gen_ruggedness
#'
#' Aggregates and transforms the ruggedness data to PRIO-GRID.
#'
#' @param input_folder path to [pg-folder].
#' @param interpolate_missing if `TRUE`, interpolates data to grid cells with missing values around coastline.
#'
#' @return a dataframe with x, y, and ruggedness
#' @export
gen_ruggedness <- function(input_folder, interpolate_missing = FALSE){
  rugged <- raster::raster(file.path(input_folder, "ruggedness", "data", "ruggedness1K.tif"))
  rugged <- raster::aggregate(rugged, fact = 10, fun = "mean") # aggregate data before reprojection to speed up.
  rast <- raster::projectRaster(from = rugged, crs = priogrid::prio_crs())
  rast <- priogrid::raster_to_pg(rast)
  names(rast) <- "ruggedness"
  ruggedness <- priogrid::raster_to_tibble(rast, add_pg_index = TRUE)

  if(interpolate_missing) {
    missing <- priogrid::missing_in_pg(ruggedness, "ruggedness", input_folder, plot_missing = FALSE)
    assertthat::assert_that(!is.null(missing))

    interpol <- priogrid::interpolate_crossection(ruggedness, variable = "ruggedness", lon = "x", lat = "y", input_folder)
    halo <- dplyr::left_join(missing, interpol, by = c("x", "y"))
    ruggedness <- dplyr::bind_rows(ruggedness, halo)

    missing <- priogrid::missing_in_pg(rg_ipol, "ruggedness", input_folder)
    assertthat::assert_that(is.null(missing))
  }

  return(ruggedness)
}
