#' Reads Worldpop Migration Flows data
#'
#' Note: The shapefile for international flowlines does not seem to work.
#'
#' @return list
#' @export
#'
read_worldpop_migration_flows <- function(print_metadata = FALSE){
  zip_file <- get_pgfile(source_name = "WorldPop Migration Flows",
                  source_version = "2019",
                  id = "5daf4962-3f07-408e-8e63-c1d7f8803070")

  shapefiles <- unzip(zip_file, list = T) |> dplyr::filter(grepl(".shp$", Name))
  csvfiles <- unzip(zip_file, list = T) |> dplyr::filter(grepl(".csv$", Name))
  metadata <- unzip(zip_file, list = T) |> dplyr::filter(grepl("Metadata", Name))

  if(!file.exists(file.path(dirname(zip_file), unzip(zip_file, list = T)$Name)) |> all()){
    unzip(zip_file, exdir = dirname(zip_file))
  }

  centroids <- sf::read_sf(file.path(dirname(zip_file), shapefiles[1, "Name"]))
  flowlines_internal <- sf::read_sf(file.path(dirname(zip_file), shapefiles[2,"Name"]))
  # flowlines_international <- sf::read_sf(file.path(dirname(zip_file), shapefiles[3,"Name"])) # This does not work.

  migration_internal <- readr::read_csv(file.path(dirname(zip_file), csvfiles[1, "Name"]))
  # migration_international <- readr::read_csv(file.path(dirname(zip_file), csvfiles[2, "Name"]))


  if(print_metadata){
    readLines(file.path(dirname(zip_file), metadata[1, "Name"]), warn = F)
    readLines(file.path(dirname(zip_file), metadata[2, "Name"]), warn = F)
  }

  list("centroids" = centroids,
       "flowlines_internal" = flowlines_internal,
       "migration_internal" = migration_internal)
}
