#' Reads the Global Multi-resolution Terrain Elevation Data (GMTED2010)
#' Unzips the folder and reads in the .shp file as sf object
#'
#' @return an object of class sf
#' @export
#'
#' @references
#' \insertRef{danielsonGlobalMultiresolutionTerrain2011}{priogrid}
read_ruggedterrain <- function() {
  zip_file <- get_pgfile(source_name = "Global Multi-resolution Terrain Elevation Data",
                  source_version = "GMTED2010",
                  id = "8c8192eb-cc29-4598-8f8a-ec190ba35c2d")

  unzip_to <- file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)))
  unzip(zip_file, exdir = unzip_to)
  return(sf::st_read(file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)), "GMTED2010_Spatial_Metadata/GMTED2010_Spatial_Metadata.shp")))

}

#' Generate elevation in PRIO-GRID format with set variable
#'
#' Takes either the min, max, mean or sdev of elevation in each PRIO-GRID cell
#'
#' Returns the value of elevation
#'
#' @param variable Variable indicating elevation function.
#' "elevation_min", "elevation_max", "elevation_mean", "elevation_sdev"
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # rt <- gen_ruggedterrain_variable(variable = "elevation_mean")
#'
#'
#' @references
#' \insertRef{danielsonGlobalMultiresolutionTerrain2011}{priogrid}
gen_ruggedterrain_variable <- function(variable) {
  rt <- read_ruggedterrain()
  pg <- prio_blank_grid()

  if (!variable %in% c("elevation_min", "elevation_max", "elevation_mean", "elevation_sdev")) {
    stop("Invalid variable. Choose from 'elevation_min', 'elevation_max', 'elevation_mean', 'elevation_sdev'.")
  }

  rt <- rt |>
    dplyr::rename(elevation_min = MIN_ELEV,
                  elevation_max = MAX_ELEV,
                  elevation_mean = MEAN_ELEV,
                  elevation_sdev = SDEV_ELEV) |>
    dplyr::mutate(value = .data[[variable]])

  rt_var <- terra::rasterize(rt, pg, field = "value", background = NA)

  return(rt_var)
}
