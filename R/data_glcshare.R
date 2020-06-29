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
#' @return a dataframe with x, y, and land type share
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
  rm(rast)
  unlink(tdir, recursive = T)
  return(df)
}

#' gen_glcs_artificial
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "artificial surfaces" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share artificial surfaces
#' @export
gen_glcs_artificial <- function(input_folder) gen_glcshare(input_folder, land_type = "artificial_surfaces")

#' gen_glcs_cropland
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "cropland" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share cropland
#' @export
gen_glcs_cropland <- function(input_folder) gen_glcshare(input_folder, land_type = "cropland")

#' gen_glcs_grassland
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "grassland" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share grassland
#' @export
gen_glcs_grassland <- function(input_folder) gen_glcshare(input_folder, land_type = "grassland")

#' gen_glcs_trees
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "trees" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share trees
#' @export
gen_glcs_trees <- function(input_folder) function(input_folder) gen_glcshare(input_folder, land_type = "trees")

#' gen_glcs_shrubs
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "shrubs" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share shrubs
#' @export
gen_glcs_shrubs <- function(input_folder) gen_glcshare(input_folder, land_type = "shrubs")

#' gen_glcs_herbaceous
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "herbaceous" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share herbaceous
#' @export
gen_glcs_herbaceous <- function(input_folder) gen_glcshare(input_folder, land_type = "herbaceous")

#' gen_glcs_mangroves
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "mangroves" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share mangroves
#' @export
gen_glcs_mangroves <- function(input_folder) gen_glcshare(input_folder, land_type = "mangroves")

#' gen_glcs_sparse
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "sparse" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share sparse
#' @export
gen_glcs_sparse <- function(input_folder) gen_glcshare(input_folder, land_type = "sparse")

#' gen_glcs_bare
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "bare_soil" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share bare_soil
#' @export
gen_glcs_bare <- function(input_folder) gen_glcshare(input_folder, land_type = "bare_soil")

#' gen_glcs_snow
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "snow" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share snow
#' @export
gen_glcs_snow <- function(input_folder) gen_glcshare(input_folder, land_type = "snow")

#' gen_glcs_water
#'
#' Aggregates and transforms the FAO Global Land Cover Share Database (2013) "water" to PRIO-GRID.
#'
#' Link to original data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47948
#'
#' Please cite: John Latham, Renato Cumani, Ilaria Rosati and Mario Bloise (2014) FAO Global Land Cover (GLC-SHARE) Beta-Release 1.0 Database, Land and Water Division, J, available at: http://www.fao.org/uploads/media/glc-share-doc.pdf.
#'
#' @param input_folder
#'
#' @return a dataframe with x, y, and share water
#' @export
gen_glcs_water <- function(input_folder) gen_glcshare(input_folder, land_type = "water")

