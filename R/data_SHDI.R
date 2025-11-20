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

  df <- df |> dplyr::rename_all(tolower)

  return(df)
}

#' Read the GDL SHDI shapefile data
#'
#' Reads the downloaded SHDI dataset from GlobalDataLab.
#'
#' @return A tibble with SHDI data.
#' @export
#'
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
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
  unlink(tmpdir)

  return(shp)
}


gen_shdi <- function(shdi = read_shdi(), shp = read_shdi_shapefile(), variable = "shdi", fun = "mean") {
  no_subnat <- shdi |>
    dplyr::group_by(country) |>
    dplyr::summarize(n_unique = length(unique(gdlcode))) |>
    dplyr::filter(n_unique == 1)

  shdi_national <- shdi |>
    dplyr::filter(level == "National", country %in% no_subnat$country) |>
    dplyr::select(year, gdlcode, !!variable)

  shdi <- shdi |>
    dplyr::filter(level == "Subnat") |>
    dplyr::select(year, gdlcode, !!variable)

  shdi <- dplyr::bind_rows(shdi_national, shdi)
  shdi <- dplyr::left_join(shdi, shp, by = "gdlcode") |> sf::st_as_sf()

  pg <- prio_blank_grid()

}
