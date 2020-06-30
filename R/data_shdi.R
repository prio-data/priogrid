
#' Generate Expected years schooling children aged 6 variable from SHDI
#'
#' Average Expected years schooling children aged 6 within each grid cell.
#'
#' @param input_folder path to SHDI data.
#' @param variable one of c("msch", "esch", "lifexp", "lgnic", "shdi")
#' @export
gen_shdi <- function(input_folder, variable = "shdi"){

  shape_zip <- file.path(input_folder, "shdi", "data", "GDL Shapefiles V4.zip")

  tdir <- tempdir()
  unzip(shape_zip, exdir = tdir)
  shp_fname <- list.files(tdir, pattern = "*.shp")
  shapefile <- file.path(tdir, shp_fname)
  geom <- sf::read_sf(shapefile)
  unlink(tdir)

  geom <- geom %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(gdlcode, geometry)

  old_geom_file <- file.path(input_folder, "shdi", "data", "GDL-SHDI-SHP-2.shp")
  old_geom <- sf::read_sf(old_geom_file) %>% dplyr::rename_all(tolower)

  shdi <- read.csv(file.path(input_folder, "shdi", "data", "SHDI Complete 4.0 (1).csv"))

  shdi <- shdi %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, gdlcode, !!variable)

  shdi <- dplyr::left_join(shdi, geom, by = "gdlcode") %>% sf::st_as_sf()

  shdi$empty <- sf::st_is_empty(shdi)
  shdi_empty <- shdi %>% dplyr::filter(empty)
  sf::st_geometry(shdi_empty) <- NULL
  shdi_empty %>% dplyr::select(country, region, gdlcode) %>% dplyr::distinct()
  missing_geometries <- shdi_empty %>% dplyr::select(country, region, gdlcode) %>% dplyr::distinct()
  missing_in_old <- old_geom %>% dplyr::filter(gdlcode %in% missing_geometries$gdlcode) %>% dplyr::select(gdlcode, geometry)
  shdi_empty <- dplyr::left_join(shdi_empty, missing_in_old, by = "gdlcode")


  shdi <- shdi %>% dplyr::filter(!empty)

  cshp <- cshapes::cshp(date = as.Date("2015-01-01")) %>% sf::st_as_sf()
  twn <- cshapes::cshp(date = as.Date("2015-01-01")) %>% sf::st_as_sf() %>% dplyr::filter(CNTRY_NAME == "Taiwan")
  twn$gdlcode <- "CHNr133"
  twn <- twn %>% dplyr::select(gdlcode, geometry)

  df <- shdi
  time_fact <- factor(df[[timevar]])

  sdf <- dplyr::select(df, !!variable)
  sdf_list <- base::split(sdf, time_fact, sep = "_")

  raster::rasterize(as(sdf_list[[1]], "Spatial"))
  as(polys, "Spatial")

  shdi <- priogrid::panel_to_pg(shdi, timevar = "year", variable = variable, need_aggregation = TRUE, missval = -1, fun = "mean")
  return(shdi)
}

#' @export
gen_msch <- function(input_folder){ gen_shdi(input_folder, variable = "msch") }

#' @export
gen_esch <- function(input_folder){ gen_shdi(input_folder, variable = "esch") }

#' @export
gen_lifexp <- function(input_folder){ gen_shdi(input_folder, variable = "lifexp") }

#' @export
gen_lgnic <- function(input_folder){ gen_shdi(input_folder, variable = "lgnic") }

