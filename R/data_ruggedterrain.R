

read_ruggedterrain <- function() {
  zip_file <- get_pgfile(source_name = "Global Multi-resolution Terrain Elevation Data",
                  source_version = "GMTED2010",
                  id = "8c8192eb-cc29-4598-8f8a-ec190ba35c2d")

  unzip_to <- file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)))
  unzip(zip_file, exdir = unzip_to)
  return(sf::st_read(file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)), "GMTED2010_Spatial_Metadata.shp")))

}


