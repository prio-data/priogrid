#' shdi
#'
#' Gives the subnational human development index (SHDI) within each grid cell, based on SHDI data v. 4.
#' The index is an average of values along three dimensions: education, health, and standard of living. Available from 1990 to 2018.
#'
#' Links to original data: https://globaldatalab.org/shdi/shapefiles/ and https://globaldatalab.org/shdi/download_files/.
#'
#' Please cite: Smits, J., Permanyer, I. The Subnational Human Development Database. Sci Data 6, 190038 (2019). https://doi.org/10.1038/sdata.2019.38
#'
#' @param input_folder path to [pg-folder].
#' @param variable one of c("shdi", msch", "esch", "lifexp", "lgnic").
#' @param add_missing_geometries if `TRUE`, adds geometries that are missing in the original data. Replacement geometries are collected from cshapes or GADM (https://gadm.org/maps.html).
#'
#' @export
gen_shdi <- function(input_folder, variable = "shdi", add_missing_geometries = TRUE){

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

  shdi <- read.csv(file.path(input_folder, "shdi", "data", "SHDI-SGDI-Total 5.0.csv"))
  shdi <- shdi %>% dplyr::rename_all(tolower)

  # Countries without subnational data
  no_subnat <- shdi %>% dplyr::group_by(country) %>% dplyr::summarize(n_unique = length(unique(gdlcode))) %>% dplyr::filter(n_unique == 1)

  shdi_national <- shdi %>%
    dplyr::filter(level == "National", country %in% no_subnat$country) %>%
    dplyr::select(year, gdlcode, !!variable)

  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, gdlcode, !!variable)

  shdi <- dplyr::bind_rows(shdi_national, shdi)
  shdi <- dplyr::left_join(shdi, geom, by = "gdlcode") %>% sf::st_as_sf()

  if(add_missing_geometries){
    shdi$empty <- sf::st_is_empty(shdi)
    shdi_empty <- shdi %>% dplyr::filter(empty)
    sf::st_geometry(shdi_empty) <- NULL

    path_miss <- file.path(input_folder, "shdi", "data", "missing_geom")
    miss <- file.path(path_miss, list.files(path_miss, pattern = "*.rds"))
    missing_files <- lapply(miss, readRDS)

    #Czech Republic
    miss_1 <- missing_files[[1]] %>% dplyr::filter(NAME_1 == "Prague") %>% dplyr::mutate(gdlcode = "CZEr101") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    #Spain
    miss_2 <- missing_files[[2]] %>% dplyr::filter(NAME_4 == "Ceuta") %>% dplyr::mutate(gdlcode = "ESPr117") %>% dplyr::select(gdlcode, geometry)
    miss_3 <- missing_files[[2]] %>% dplyr::filter(NAME_4 == "Melilla") %>% dplyr::mutate(gdlcode = "ESPr118") %>% dplyr::select(gdlcode, geometry)
    #Fiji
    miss_4 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Naitasiri") %>% dplyr::mutate(gdlcode = "FJIr101") %>% dplyr::select(gdlcode, geometry)
    miss_5 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Rewa") %>% dplyr::mutate(gdlcode = "FJIr102") %>% dplyr::select(gdlcode, geometry)
    miss_6 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Serua" | NAME_2 == "Namosi")
    miss_6 <- sf::st_sf(geometry = sf::st_union(miss_6)) %>% dplyr::mutate(gdlcode = "FJIr103") %>% dplyr::select(gdlcode, geometry)
    miss_7 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Tailevu") %>% dplyr::mutate(gdlcode = "FJIr104") %>% dplyr::select(gdlcode, geometry)
    miss_8 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Kadavu" | NAME_2 == "Lau" | NAME_2 == "Lomaiviti" | NAME_2 == "Rotuma")
    miss_8 <- sf::st_sf(geometry = sf::st_union(miss_8)) %>% dplyr::mutate(gdlcode = "FJIr105") %>% dplyr::select(gdlcode, geometry) # st_union or st_combine?
    miss_9 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Cakaudrove" | NAME_2 == "Bua")
    miss_9 <- sf::st_sf(geometry = sf::st_union(miss_9)) %>% dplyr::mutate(gdlcode = "FJIr106") %>% dplyr::select(gdlcode, geometry)
    miss_10 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Macuata") %>% dplyr::mutate(gdlcode = "FJIr107") %>% dplyr::select(gdlcode, geometry)
    miss_11 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Ba") %>% dplyr::mutate(gdlcode = "FJIr108") %>% dplyr::select(gdlcode, geometry)
    miss_12 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Nadroga/Navosa") %>% dplyr::mutate(gdlcode = "FJIr109") %>% dplyr::select(gdlcode, geometry)
    miss_13 <- missing_files[[3]] %>% dplyr::filter(NAME_2 == "Ra") %>% dplyr::mutate(gdlcode = "FJIr110") %>% dplyr::select(gdlcode, geometry)
    #France, Guadeloupe
    miss_14 <- missing_files[[4]] %>% dplyr::mutate(gdlcode = "FRAr125") %>% dplyr::select(gdlcode, geometry)
    #Italy
    miss_15 <- missing_files[[5]] %>% dplyr::filter(NAME_2 == "Bolzano") %>% dplyr::mutate(gdlcode = "ITAr113") %>% dplyr::select(gdlcode, geometry)
    #Kazakhstan
    miss_16 <- missing_files[[6]] %>% dplyr::filter(stringr::str_detect(NAME_2, "Almaty")) %>% dplyr::mutate(gdlcode = "KAZr101") %>% dplyr::select(gdlcode, geometry)
    #Kyrgyzstan
    miss_17 <- missing_files[[7]] %>% dplyr::filter(NAME_1 == "Jalal-Abad") %>% dplyr::mutate(gdlcode = "KGZr105") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_18 <- missing_files[[7]] %>% dplyr::filter(NAME_1 == "Osh") %>% dplyr::mutate(gdlcode = "KGZr106") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_19 <- missing_files[[7]] %>% dplyr::filter(NAME_1 == "Naryn") %>% dplyr::mutate(gdlcode = "KGZr107") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_20 <- missing_files[[7]] %>% dplyr::filter(NAME_1 == "Batken") %>% dplyr::mutate(gdlcode = "KGZr108") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    #Malta
    miss_21 <- missing_files[[8]] %>% dplyr::filter(VARNAME_1 == "Gozo") %>% dplyr::mutate(gdlcode = "MLTr102") %>% dplyr::select(gdlcode, geometry)
    miss_22 <- missing_files[[8]] %>% dplyr::filter(VARNAME_1 != "Gozo")
    miss_22 <- sf::st_sf(geometry = sf::st_union(miss_22)) %>% dplyr::mutate(gdlcode = "MLTr101") %>% dplyr::select(gdlcode, geometry)
    #France, Martinique
    miss_23 <- missing_files[[9]] %>% dplyr::mutate(gdlcode = "FRAr126") %>% dplyr::select(gdlcode, geometry)
    #France, Reunion
    miss_24 <- missing_files[[10]] %>% dplyr::mutate(gdlcode = "FRAr128") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    #Tajikistan
    miss_25 <- missing_files[[11]] %>% dplyr::filter(NAME_1 == "Dushanbe") %>% dplyr::mutate(gdlcode = "TJKr101") %>% dplyr::select(gdlcode, geometry)
    #Indonesia, Timor Leste/East Timor
    miss_26 <- missing_files[[12]] %>% dplyr::mutate(gdlcode = "IDNr119") %>% dplyr::select(gdlcode, geometry)
    #Vanuatu
    miss_27 <- missing_files[[13]] %>% dplyr::filter(NAME_2 == "Port Vila") %>% dplyr::mutate(gdlcode = "VUTr107") %>% dplyr::select(gdlcode, geometry)
    miss_28 <- missing_files[[13]] %>% dplyr::filter(NAME_2 == "Luganville") %>% dplyr::mutate(gdlcode = "VUTr108") %>% dplyr::select(gdlcode, geometry)
    #Kosovo
    miss_29 <- missing_files[[14]] %>% dplyr::filter(stringr::str_detect(NAME_1, "kovica")) %>% dplyr::mutate(gdlcode = "KSVr101") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_30 <- missing_files[[14]] %>% dplyr::filter(NAME_1 == "Gnjilane") %>% dplyr::mutate(gdlcode = "KSVr102") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_31 <- missing_files[[14]] %>% dplyr::filter(NAME_1 == "Kosovska Mitrovica") %>% dplyr::mutate(gdlcode = "KSVr103") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_32 <- missing_files[[14]] %>% dplyr::filter(stringr::str_detect(NAME_1, "Pe")) %>% dplyr::mutate(gdlcode = "KSVr104") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_33 <- missing_files[[14]] %>% dplyr::filter(NAME_1 == "Prizren") %>% dplyr::mutate(gdlcode = "KSVr105") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_34 <- missing_files[[14]] %>% dplyr::filter(NAME_1 == "Pristina") %>% dplyr::mutate(gdlcode = "KSVr106") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    miss_35 <- missing_files[[14]] %>% dplyr::filter(stringr::str_detect(NAME_1, "Uro")) %>% dplyr::mutate(gdlcode = "KSVr107") %>% dplyr::select(gdlcode, geometry) %>% sf::st_cast(to = "MULTIPOLYGON")
    #China, Taiwan
    miss_36 <- cshapes::cshp(date = as.Date("2015-01-01")) %>% sf::st_as_sf() %>% dplyr::filter(CNTRY_NAME == "Taiwan") %>%
      dplyr::mutate(gdlcode = "CHNr133") %>% dplyr::select(gdlcode, geometry)

    z <- as.list(mget(paste0("miss_", 1:36)))
    miss <- data.table::rbindlist(z) %>% sf::st_as_sf()

    shdi_missing <- dplyr::inner_join(miss, shdi_empty, by = "gdlcode")

    shdi <- shdi %>% dplyr::filter(!empty)

    shdi <- rbind(shdi, shdi_missing)

    shdi <- shdi %>% dplyr::arrange(gdlcode, year) %>% dplyr::select(-empty)
  }

  shdi_sum <- priogrid::panel_to_pg(shdi, timevar = "year", variable = variable, need_aggregation = TRUE, missval = -1, fun = "sum")
  shdi_count <- priogrid::panel_to_pg(shdi, timevar = "year", variable = variable, need_aggregation = TRUE, missval = -1, fun = "count")
  shdi_count <- shdi_count %>% dplyr::rename("count" = !!variable)
  shdi_sum <- dplyr::left_join(shdi_sum, shdi_count, by = c("x", "y", "pgid", "year"))

  shdi_sum[[variable]] <- shdi_sum[[variable]] / shdi_sum$count
  shdi_sum$count <- NULL
  shdi_sum$year <- as.numeric(shdi_sum$year)
  return(shdi_sum)
}


#' msch
#'
#' Gives the mean years of schooling for population aged 25+ within each grid cell, based on SHDI data v. 4. Available from 1990 to 2018.
#'
#' Links to original data: https://globaldatalab.org/shdi/shapefiles/ and https://globaldatalab.org/shdi/download_files/.
#'
#' Please cite: Smits, J., Permanyer, I. The Subnational Human Development Database. Sci Data 6, 190038 (2019). https://doi.org/10.1038/sdata.2019.38
#'
#' @param input_folder path to [pg-folder].
#' @param add_missing_geometries if `TRUE`, adds geometries that are missing in the original data. Replacement geometries are collected from cshapes or GADM (https://gadm.org/maps.html).
#'
#' @export
gen_msch <- function(input_folder, add_missing_geometries = TRUE){ gen_shdi(input_folder, variable = "msch", add_missing_geometries = add_missing_geometries) }


#' esch
#'
#' Gives the expected years of schooling for children aged 6 within each grid cell, based on SHDI data v. 4. Available from 1990 to 2018.
#'
#' Links to original data: https://globaldatalab.org/shdi/shapefiles/ and https://globaldatalab.org/shdi/download_files/.
#'
#' Please cite: Smits, J., Permanyer, I. The Subnational Human Development Database. Sci Data 6, 190038 (2019). https://doi.org/10.1038/sdata.2019.38
#'
#' @param input_folder path to [pg-folder].
#' @param add_missing_geometries if `TRUE`, adds geometries that are missing in the original data. Replacement geometries are collected from cshapes or GADM (https://gadm.org/maps.html).
#'
#' @export
gen_esch <- function(input_folder, add_missing_geometries = TRUE){ gen_shdi(input_folder, variable = "esch", add_missing_geometries = add_missing_geometries) }


#' lifexp
#'
#' Gives the life expectancy at birth within each grid cell, based on SHDI data v. 4. Available from 1990 to 2018.
#'
#' Links to original data: https://globaldatalab.org/shdi/shapefiles/ and https://globaldatalab.org/shdi/download_files/.
#'
#' Please cite: Smits, J., Permanyer, I. The Subnational Human Development Database. Sci Data 6, 190038 (2019). https://doi.org/10.1038/sdata.2019.38
#'
#' @param input_folder path to [pg-folder].
#' @param add_missing_geometries if `TRUE`, adds geometries that are missing in the original data. Replacement geometries are collected from cshapes or GADM (https://gadm.org/maps.html).
#'
#' @export
gen_lifexp <- function(input_folder, add_missing_geometries = TRUE){ gen_shdi(input_folder, variable = "lifexp", add_missing_geometries = add_missing_geometries) }


#' gnic
#'
#' Gives the Gross National Income per capita in thousands of USD (2011 PPP) within each grid cell, based on SHDI data v. 4. Available from 1990 to 2018.
#'
#' Links to original data: https://globaldatalab.org/shdi/shapefiles/ and https://globaldatalab.org/shdi/download_files/.
#'
#' Please cite: Smits, J., Permanyer, I. The Subnational Human Development Database. Sci Data 6, 190038 (2019). https://doi.org/10.1038/sdata.2019.38
#'
#' @param input_folder path to [pg-folder].
#' @param add_missing_geometries if `TRUE`, adds geometries that are missing in the original data. Replacement geometries are collected from cshapes or GADM (https://gadm.org/maps.html).
#'
#' @export
gen_gnic <- function(input_folder, add_missing_geometries = TRUE){ gen_shdi(input_folder, variable = "gnic", add_missing_geometries = add_missing_geometries) }

