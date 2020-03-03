## TODO Merge on country code?



#' Generate gcp_mer variable
#'
#' Generate gross cell product (USD) variable, based on the G-Econ dataset v. 4.0.
#'
#' @param gecon_data G-Econ v. 4.0 excel data

gen_gcp_mer <- function(input_folder, variable = "GCP_MER"){
  gcp <- readxl::read_xls(file.path(input_folder, "Gecon40_post_final.xls"), sheet = 1, progress = FALSE)

  suffix <- sub("GCP_", "", variable)

  gcp <- gcp %>%
    dplyr::select(LAT, LONGITUDE, AREA, MER1990_40, MER1995_40, MER2000_40, MER2005_40, PPP1990_40, PPP1995_40, PPP2000_40, PPP2005_40) %>%
    dplyr::filter(AREA > 0) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with(suffix)) %>%
    dplyr::group_by(LAT, LONGITUDE, name) %>%
    dplyr::mutate(area_share = AREA / sum(AREA, na.rm = T)) %>%
    dplyr::mutate(value = log(value+1)*area_share) %>%
    dplyr::summarise(value = exp(sum(value, na.rm = T))-1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(grepl(suffix, name))

  gcp$year <- as.numeric(sub("_40", "", sub(suffix, "", gcp$name)))
  time_fact <- factor(gcp$year)

  gcp_list <- dplyr::select(gcp, LONGITUDE, LAT, value)
  gcp_list <- base::split(gcp_list, time_fact, sep = "_")
  rast_list <- parallel::mclapply(gcp_list, raster::rasterFromXYZ, crs = priogrid::prio_crs())
  rast_list <- parallel::mclapply(rast_list, priogrid::rasterextent_to_pg)
  rast_list <- parallel::mclapply(rast_list, priogrid::raster_to_pg)
  pg_tibble <- parallel::mclapply(rast_list, priogrid::raster_to_tibble, add_pg_index = TRUE)

  add_timevar <- function(df, time, timevar){
    df[[timevar]] <- time
    return(df)
  }

  pg_tibble <- purrr::map2_dfr(pg_tibble, names(pg_tibble), add_timevar, timevar = "year")

  names(pg_tibble) <- c("x", "y", tolower(variable), "pgid", "year")

  return(pg_tibble)
}




#' Generate gcp_ppp variable
#'
#' Generate gross cell product (USD adjusted for purchasing-power-parity) variable,
#' based on the G-Econ dataset v. 4.0.
#'
#' @param input_folder
gen_gcp_ppp <- function(input_folder){
  gen_gcp_mer(input_folder, variable = "GCP_PPP")
}


#' Generate gcp_qual variable
#'
#' Generate quality of GCP values variable,
#' based on the G-Econ dataset v. 4.0.
#'
#' @param input_folder
gen_gcp_qual <- function(input_folder){
  gcp <- readxl::read_xls(file.path(input_folder, "Gecon40_post_final.xls"), sheet = 1, progress = FALSE)

  gcp <- gcp %>%
    dplyr::select(LAT, LONGITUDE, QUALITY) %>%
    dplyr::group_by(LAT, LONGITUDE) %>%
    dplyr::summarise(quality = mean(QUALITY, na.rm = T))

  gcp <- dplyr::select(gcp, LONGITUDE, LAT, quality)
  gcp$quality <- dplyr::if_else(gcp$quality <= -499, NA_real_, gcp$quality)

  pg_tibble <- raster::rasterFromXYZ(gcp, crs = priogrid::prio_crs()) %>%
    priogrid::rasterextent_to_pg() %>%
    priogrid::raster_to_pg() %>%
    priogrid::raster_to_tibble(add_pg_index = T)
  return(pg_tibble)
}


