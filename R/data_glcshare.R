#' gen_glcshare
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#' @param land_type one of c("artificial_surfaces", "cropland", "grassland", "trees", "shrubs", "herbaceous", "mangroves", "sparse", "bare_soil", "snow", "water")
#'
#' @return a dataframe with x, y, and gmted2010 elevation
#' @export
gen_glcshare <- function(input_folder, land_type){

  land_types <- c("artificial_surfaces",
                  "cropland",
                  "grassland",
                  "trees",
                  "shrubs",
                  "herbaceous",
                  "mangroves",
                  "sparse",
                  "bare_soil",
                  "snow",
                  "water")
  land_type_fname <- paste0("GlcShare_v10_", stringr::str_pad(1:11, 2, pad = "0"), ".zip")
  land_types <- dplyr::tibble("type" = land_types, "fname" = land_type_fname)

  zip_fname <- dplyr::filter(land_types, type == land_type) %>% dplyr::pull(fname)
  zipfile <- file.path(input_folder, "glcshare", "data", zip_fname)

  tdir <- tempdir()
  unzip(zipfile, exdir = tdir)
  tif_fname <- list.files(tdir, pattern = "*.Tif")
  rastfile <- file.path(tdir, tif_fname)

  rast <- raster::raster(rastfile)
  raster::crs(rast) <- priogrid::prio_crs()
  rast <- priogrid::raster_to_pg(rast)

  names(rast) <- paste0("glcs_", land_type)
  df <- priogrid::raster_to_tibble(rast,add_pg_index = TRUE)
  unlink(tdir, recursive = T)
  return(df)
}
