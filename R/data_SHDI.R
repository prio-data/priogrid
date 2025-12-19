#' Read GlobalDataLab Subnational Human Development Index (SHDI) Data
#'
#' This function reads the GlobalDataLab Subnational Human Development Index (SHDI)
#' dataset (version 7.0) from local storage using PRIO-GRID file management.
#' The dataset is returned as a cleaned tabular object with standardized
#' lower-case column names.
#'
#' @return A \code{data.frame} (tibble-compatible) containing SHDI values
#'   across countries, subnational units, and years as provided by
#'   GlobalDataLab.
#'
#' @examples
#' \dontrun{
#' shdi <- read_shdi()
#' }
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval and storage management
#'
#' @export
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

#' Read geoBoundaries Administrative Boundary Data
#'
#' This function reads administrative boundary shapefiles from the
#' geoBoundaries global database (version 5.0.0). The data are unzipped
#' to a temporary directory and returned as an \code{sf} object.
#'
#' @return An \code{sf} object containing global administrative boundary
#'   geometries from geoBoundaries.
#'
#' @examples
#' \dontrun{
#' gb <- read_geoboundaries()
#' }
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval,
#' \code{\link[sf]{st_read}} for spatial data input
#'
#' @export
#' @references
#' \insertRef{runfolaGeoBoundariesGlobalDatabase2020}{priogrid}
read_geoboundaries <- function() {
  f <- get_pgfile(source_name = "geoBoundaries",
                  source_version = "5.0.0",
                  id = "a8e35e36-9f7e-4194-9cc4-ce8ca59f7b51")

  out_dir <- tempfile("gb_")
  dir.create(out_dir)
  unzip(f, exdir = out_dir)

  shp_files <- list.files(out_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

  shp_path <- shp_files[1]

  gb <- sf::st_read(shp_path)

  return(gb)
}

#' Read GlobalDataLab SHDI Shapefile Data
#'
#' This function reads the GlobalDataLab Subnational Human Development Index (SHDI)
#' shapefile dataset (version 7.0) from local storage. The shapefile is extracted
#' from a compressed archive and returned as an \code{sf} object with subnational
#' geometries linked by GlobalDataLab codes.
#'
#' @return An \code{sf} object containing SHDI geometries with GlobalDataLab
#'   subnational identifiers.
#'
#' @examples
#' \dontrun{
#' shdi_shp <- read_shdi_shapefile()
#' }
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval,
#' \code{\link[sf]{st_read}} for spatial data input
#'
#' @export
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

#' Generate PRIO-GRID Compatible SHDI Data
#'
#' This function processes GlobalDataLab Subnational Human Development Index (SHDI)
#' data and generates a PRIO-GRID–compatible raster representation. The function
#' harmonizes national and subnational SHDI observations, resolves missing or empty
#' geometries using geoBoundaries crosswalks, and spatially rasterizes the resulting
#' data to the PRIO-GRID.
#'
#' The function may take some time to run.
#'
#' @param shdi A data frame containing SHDI data as returned by
#'   \code{\link{read_shdi}}.
#' @param shp An \code{sf} object containing SHDI geometries as returned by
#'   \code{\link{read_shdi_shapefile}}.
#' @param geoboundaries An \code{sf} object containing administrative boundaries
#'   from \code{\link{read_geoboundaries}}.
#' @param variable A character string specifying the SHDI variable to extract.
#'   Options are "msch", "esch", "lifexp", "gnic", "shdi"
#'   Default is \code{"shdi"}.
#'
#' @return An object containing PRIO-GRID–aligned SHDI data, including
#'   spatially harmonized geometries and rasterized values.
#'
#' @examples
#' \dontrun{
#' shdi_pg <- gen_shdi()
#' }
#'
#' @seealso
#' \code{\link{read_shdi}},
#' \code{\link{read_shdi_shapefile}},
#' \code{\link{read_geoboundaries}},
#' \code{\link{prio_blank_grid}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
gen_shdi <- function(shdi = read_shdi(),
                     shp  = read_shdi_shapefile(),
                     geoboundaries = read_geoboundaries(),
                     variable = "shdi") {

  no_subnat <- dplyr::group_by(shdi, country)
  no_subnat <- dplyr::summarize(no_subnat, n_unique = base::length(base::unique(gdlcode)))
  no_subnat <- dplyr::filter(no_subnat, n_unique == 1)

  shdi_national <- dplyr::filter(shdi, level == "National", country %in% no_subnat$country)
  shdi_national <- dplyr::select(shdi_national, year, gdlcode, !!variable)

  shdi_subnat <- dplyr::filter(shdi, level == "Subnat")
  shdi_subnat <- dplyr::select(shdi_subnat, year, gdlcode, !!variable)

  shdi_both <- dplyr::bind_rows(shdi_national, shdi_subnat)
  shdi_both <- dplyr::left_join(shdi_both, shp, by = "gdlcode")
  shdi_both <- sf::st_as_sf(shdi_both)
  shdi_both$empty <- sf::st_is_empty(shdi_both)

  shdi_empty    <- dplyr::filter(shdi_both, empty)
  shdi_nonempty <- dplyr::filter(shdi_both, !empty)

  if (nrow(shdi_empty) == 0) return(dplyr::select(shdi_both, -empty))

  crosswalk <- tibble::tibble(
    gdlcode = c(
      # Belgium
      "BELr105",
      # Spain (Ceuta, Melilla)
      "ESPr117","ESPr118",
      # Maldives (ADM1 atolls)
      "MDVr101","MDVr102","MDVr103","MDVr104","MDVr105","MDVr106",
      # Tonga (ADM1 island groups)
      "TONr101","TONr102","TONr103","TONr104","TONr105",
      # Samoa (ADM1 districts)
      "WSMr101","WSMr102","WSMr103","WSMr104"
    ),
    iso = c(
      "BEL",
      "ESP","ESP",
      "MDV","MDV","MDV","MDV","MDV","MDV",
      "TON","TON","TON","TON","TON",
      "WSM","WSM","WSM","WSM"
    ),
    adm_level = c(
      "ADM1",
      "ADM1","ADM1",
      "ADM1","ADM1","ADM1","ADM1","ADM1","ADM1",
      "ADM1","ADM1","ADM1","ADM1","ADM1",
      "ADM1","ADM1","ADM1","ADM1"
    ),
    target_name = c(
      # Belgium
      "Brussels Hoofdstedelijk",
      # Spain
      "Ciudad Autónoma de Ceuta","Ciudad Autónoma de Melilla",
      # Maldives
      "Haa Alif","Haa Dhaalu","Shaviyani","Lhaviyani","Raa","Baa",
      # Tonga (geoboundaries ADM1 names)
      "Tongatapu","Vava'u","Ha'apai","Niuas","'Eua",
      # Samoa (geoboundaries ADM1 names)
      "Tuamasaga","A'ana","Atua","Gaga'emauga"
    )
  )

  needed <- base::sort(base::unique(shdi_empty$gdlcode))
  xwalk_needed <- dplyr::filter(crosswalk, gdlcode %in% needed)

  repl_list <- base::lapply(seq_len(nrow(xwalk_needed)), function(i) {
    row <- xwalk_needed[i, ]
    gb_sub <- dplyr::filter(geoboundaries,
                            .data$shapeGroup == row$iso,
                            .data$shapeType  == row$adm_level,
                            .data$shapeName  == row$target_name)
    gb_sub <- dplyr::select(gb_sub, geometry)
    gb_sub <- dplyr::mutate(gb_sub, gdlcode = row$gdlcode)
    gb_sub <- gb_sub[, c("gdlcode","geometry")]
    gb_sub
  })

  replacements <- dplyr::bind_rows(repl_list)
  replacements <- sf::st_cast(replacements, "MULTIPOLYGON")
  replacements <- sf::st_make_valid(replacements)

  shdi_missing_filled <- dplyr::inner_join(
    sf::st_drop_geometry(shdi_empty),
    replacements,
    by = "gdlcode"
  )
  shdi_missing_filled <- sf::st_as_sf(shdi_missing_filled)

  out <- dplyr::bind_rows(shdi_nonempty, shdi_missing_filled)
  out <- dplyr::arrange(out, gdlcode, year)
  out <- dplyr::select(out, -empty)
  shdi <- out


  pg <- prio_blank_grid()

  coversh <- exactextractr::exact_extract(pg, shdi, include_cols = variable)
  ra <- exactextractr::rasterize_polygons(shdi, pg)
  pg <- pg*ra # Remove non-land cells
  res <- terra::classify(pg, coversh[[1]])

  names(res) <- "SHDI"

  return(res)

}

