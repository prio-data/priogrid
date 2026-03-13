#' Read GlobalDataLab Subnational Human Development Index (SHDI) Data
#'
#' Downloads from a PRIO hosted mirror.
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
#' shdi_csv <- read_shdi_csv()
#' }
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval and storage management
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
read_shdi_csv <- function() {
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
#' Downloads from a PRIO hosted mirror.
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

#' Reads in SHDI data and fixes empty SHDI geometries
#'
#' This function processes GlobalDataLab Subnational Human Development Index (SHDI)
#' data, resolving missing or empty geometries using geoBoundaries crosswalks, and
#' returns a sf data.frame
#'
#' @param shdi_csv A data frame containing SHDI data as returned by
#'   \code{\link{read_shdi_csv}}.
#' @param shp An \code{sf} object containing SHDI geometries as returned by
#'   \code{\link{read_shdi_shapefile}}.
#' @param geoboundaries An \code{sf} object containing administrative boundary
#'   geometries as returned by \code{\link{read_geoboundaries}}.
#' @param fix_empty A boolean whether or not to fix SHDI empty geometries. Defaults to TRUE.
#'
#' @return An \code{sf} object containing SHDI data and geometries.
#'
#' @examples
#' \dontrun{
#' # Generate PRIO-GRID SHDI
#' shdi <- read_shdi()
#' }
#'
#' @seealso
#' \code{\link{read_shdi_csv}},
#' \code{\link{read_shdi_shapefile}},
#' \code{\link{read_geoboundaries}},
#' \code{\link{prio_blank_grid}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
read_shdi <- function(shdi_csv = read_shdi_csv(),
                 shp  = read_shdi_shapefile(),
                 geoboundaries = read_geoboundaries(),
                 fix_empty = TRUE) {


  shdi <- dplyr::left_join(shdi_csv, shp |> dplyr::select(-continent, -iso_code), by = "gdlcode") |>
    sf::st_as_sf(crs = sf::st_crs(shp))

  if(!fix_empty) return(shdi)

  shdi$empty <- sf::st_is_empty(shdi)
  shdi_empty <- dplyr::filter(shdi, empty)

  if (nrow(shdi_empty) == 0) return(dplyr::select(shdi, -empty))

  shdi <- shdi |> dplyr::filter(!empty) |> dplyr::select(-empty)

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

  needed <- sort(unique(shdi_empty$gdlcode))
  xwalk_needed <- dplyr::filter(crosswalk, gdlcode %in% needed)

  repl_list <- lapply(seq_len(nrow(xwalk_needed)), function(i) {
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

  shdi_missing_filled <- dplyr::inner_join(
    sf::st_drop_geometry(shdi_empty),
    replacements,
    by = "gdlcode"
  )
  shdi_missing_filled <- sf::st_as_sf(shdi_missing_filled, crs = sf::st_crs(geoboundaries))

  shdi <- dplyr::bind_rows(shdi, shdi_missing_filled) |>
    dplyr::arrange(gdlcode, year)

  return(shdi)
}

#' Generate PRIO-GRID Compatible SHDI Variables
#'
#' This function processes GlobalDataLab Subnational Human Development Index (SHDI)
#' data and produces a PRIO-GRID–aligned raster for a selected SHDI-related variable.
#' It harmonizes national and subnational observations, resolves missing or empty
#' geometries using geoBoundaries crosswalks, and rasterizes polygon-level values
#' onto the PRIO-GRID using area-weighted aggregation.
#'
#' The function may take some time to run due to spatial operations.
#'
#' @param variable A character string specifying which SHDI-related variable to
#'   generate. Supported values include:
#'   \itemize{
#'     \item \code{"shdi"} – Subnational Human Development Index
#'     \item \code{"msch"} – Mean years of schooling
#'     \item \code{"esch"} – Expected years of schooling
#'     \item \code{"lifexp"} – Life expectancy at birth
#'     \item \code{"gnic"} – Gross national income per capita
#'   }
#'   Default is \code{"shdi"}.
#'
#' @return A \code{SpatRaster} object (from the \pkg{terra} package) aligned to the
#'   PRIO-GRID, containing the selected SHDI variable aggregated to grid cells using
#'   area-weighted means.
#'
#' @examples
#' \dontrun{
#' # Generate PRIO-GRID SHDI
#' shdi_pg <- shdi()
#'
#' # Generate PRIO-GRID life expectancy
#' lifexp_pg <- shdi(variable = "lifexp")
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
shdi <- function(variable = "shdi") {

  df <- read_shdi()

  pg_years <- lubridate::year(pg_dates())
  shdi_years <- pg_years[pg_years %in% unique(df$year)]

  for(t in 1:length(shdi_years)){
    out <- df |> dplyr::filter(year == shdi_years[t])
    pg <- prio_blank_grid()

    coversh <- exactextractr::exact_extract(pg, out, include_cols = variable)

    cmat <- dplyr::bind_rows(coversh)

    cmat <- cmat |>
      dplyr::group_by(value) |>
      dplyr::summarise("{variable}" := {vals <- .data[[variable]]
      if (all(is.na(vals))) {
        NA_real_
      } else {
        stats::weighted.mean(vals, coverage_fraction, na.rm = TRUE)
      }
      },
      .groups = "drop")

    ra <- exactextractr::rasterize_polygons(out, pg)
    ra <- terra::ifel(!is.na(ra), 1, NA)
    pg <- pg*ra # Remove non-shdi

    if(t == 1){
      r <- terra::classify(pg, cmat)
    } else{
      terra::add(r) <- terra::classify(pg, cmat)
    }
  }

  pgday <- pgoptions$get_start_date() |> lubridate::day()
  pgmonth <- pgoptions$get_start_date() |> lubridate::month()

  names(r) <- as.Date(paste(shdi_years, pgmonth, pgday, sep = "-"))
  return(r)

}


#' Generate PRIO-GRID SHDI
#'
#' Convenience wrapper around \code{\link{shdi}} that generates a PRIO-GRID–aligned
#' raster of the Subnational Human Development Index (SHDI).
#'
#' @return A \code{SpatRaster} object containing PRIO-GRID SHDI values.
#'
#' @examples
#' \dontrun{
#' shdi_pg <- gen_shdi()
#' }
#'
#' @seealso
#' \code{\link{shdi}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
gen_shdi <- function() {
  shdi <- shdi(variable = "shdi")
  return(shdi)
}

#' Generate PRIO-GRID Mean Years of Schooling
#'
#' Convenience wrapper around \code{\link{shdi}} that generates a PRIO-GRID–aligned
#' raster of mean years of schooling.
#'
#' @return A \code{SpatRaster} object containing PRIO-GRID mean years of schooling.
#'
#' @examples
#' \dontrun{
#' msch_pg <- gen_msch()
#' }
#'
#' @seealso
#' \code{\link{shdi}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
gen_msch <- function() {
  msch <- shdi(variable = "msch")
  return(shdi)
}

#' Generate PRIO-GRID Expected Years of Schooling
#'
#' Convenience wrapper around \code{\link{shdi}} that generates a PRIO-GRID–aligned
#' raster of expected years of schooling.
#'
#' @return A \code{SpatRaster} object containing PRIO-GRID expected years of schooling.
#'
#' @examples
#' \dontrun{
#' esch_pg <- gen_esch()
#' }
#'
#' @seealso
#' \code{\link{shdi}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
gen_esch <- function() {
  esch <- shdi(variable = "esch")
  return(shdi)
}

#' Generate PRIO-GRID Life Expectancy
#'
#' Convenience wrapper around \code{\link{shdi}} that generates a PRIO-GRID–aligned
#' raster of life expectancy at birth.
#'
#' @return A \code{SpatRaster} object containing PRIO-GRID life expectancy values.
#'
#' @examples
#' \dontrun{
#' lifexp_pg <- gen_lifexp()
#' }
#'
#' @seealso
#' \code{\link{shdi}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
gen_lifexp <- function() {
  lifexp <- shdi(variable = "lifexp")
  return(shdi)
}

#' Generate PRIO-GRID Gross National Income per Capita
#'
#' Convenience wrapper around \code{\link{shdi}} that generates a PRIO-GRID–aligned
#' raster of gross national income per capita.
#'
#' @return A \code{SpatRaster} object containing PRIO-GRID GNI per capita values.
#'
#' @examples
#' \dontrun{
#' gnic_pg <- gen_gnic()
#' }
#'
#' @seealso
#' \code{\link{shdi}}
#'
#' @export
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
gen_gnic <- function() {
  gnic <- shdi(variable = "gnic")
  return(shdi)
}
