#' Read the GDL SHDI data
#'
#' Reads the downloaded SHDI dataset from GlobalDataLab.
#'
#' @return A tibble with SHDI data.
#' @export
#'
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
read_shdi <- function() {
  f <- get_pgfile(source_name = "GlobalDataLab Subnational Human Development (SHDI)",
                  source_version = "v.7.0",
                  id = "8aaf6b27-6372-43da-87a9-d4235095bb2c")

  csv_path <- f[ grepl("\\.csv$", f) ][1]

  df <- read.csv(csv_path)

  return(df)
}


read_shdi_shapefile <- function() {
  files <- get_pgfile(
    source_name = "GlobalDataLab Subnational Human Development (SHDI)",
    source_version = "v.7.0",
    id = "8aaf6b27-6372-43da-87a9-d4235095bb2c")

  zip_path <- files[grepl("\\.zip$", files, ignore.case = TRUE)][1]

  zip_contents <- utils::unzip(zip_path, list = TRUE)

  shp_entry <- zip_contents$Name[grepl("\\.shp$", zip_contents$Name, ignore.case = TRUE)][1]

  tmpdir <- tempfile("shpdir")
  dir.create(tmpdir)
  base <- tools::file_path_sans_ext(basename(shp_entry))

  files_to_extract <- zip_contents$Name[grepl(paste0("^(.*/)?", base, "\\."), zip_contents$Name, ignore.case = TRUE)]

  utils::unzip(zip_path, files = files_to_extract, exdir = tmpdir)
  shp_path <- file.path(tmpdir, shp_entry)

  shp <- sf::st_read(shp_path, quiet = TRUE)

  return(shp)
}
