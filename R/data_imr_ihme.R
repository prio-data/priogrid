
#' @title imr_ihme_mean
#'
#' Generate variable of average under-5 mortality rate within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_imr_ihme_mean <- function(input_folder, fun = "mean"){
  imr <- get_ihme_brick(input_folder)

  raster::extent(imr) <- priogrid::prio_extent()

  imr_mean <- priogrid::raster_to_pg(imr, aggregation_function = fun)

  imr_mean <- priogrid::rasterextent_to_pg(imr_mean)

  imr_mean <- priogrid::raster_to_tibble(imr_mean, add_pg_index = TRUE)

  name <- paste0("imr_ihme_", as.character(fun))

  imr_mean <- imr_mean %>%
    dplyr::group_by(pgid) %>%
    rename("y2000" = 3, "y2005" = 4, "y2010" = 5, "y2015" = 6) %>%
    tidyr::pivot_longer(cols = 3:6,
                        values_to = paste0(name),
                        names_to = "year") %>%
    dplyr::mutate(year = ifelse(year == "y2000", 2000,
                                ifelse(year == "y2005", 2005,
                                       ifelse(year == "y2010", 2010,
                                              ifelse(year == "y2015", 2015, year))))) %>%
    dplyr::ungroup()

  return(imr_mean)

}


#' @title imr_ihme_sd
#'
#' Generate standard deviation under-5 mortality rate variable within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_imr_ihme_sd <- function(input_folder){
  imr_sd <- gen_imr_ihme_mean(input_folder, fun = "sd")

  return(imr_sd)
}

#' @title imr_ihme_max
#'
#' Generate variable of maximum under-5 mortality rate within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_imr_ihme_max <- function(input_folder){
  imr_max <- gen_imr_ihme_mean(input_folder, fun = "max")

  return(imr_max)
}

#' @title imr_ihme_min
#'
#' Generate variable of minimum under-5 mortality rate within each grid cell
#' based on Institute for Health Metrics and Evaluation (IHME) data (available only for Africa).
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export
gen_imr_ihme_min <- function(input_folder){
  imr_min <- gen_imr_ihme_mean(input_folder, fun = "min")

  return(imr_min)
}



prep_imr_imhe <- function(ihme_00, ihme_05, ihme_10, ihme_15){
   i00 <- raster::raster(ihme_00)
   i05 <- raster::raster(ihme_05)
   i10 <- raster::raster(ihme_10)
   i15 <- raster::raster(ihme_15)
   raster::brick(i00,i05,i10,i15)
}
# ================================================

#' get_ihme_brick
#'
#' Returns a raster brick containing the IHME data
#'
#' @param folder A folder containing the files

get_ihme_brick <- function(folder){
   do.call(prep_imr_imhe,as.list(get_ihme_files(folder)))
}

# ================================================

#' get_ihme_files
#'
#' Returns the paths to the four files needed by the functions
#' generating the ihme raster
#'
#' @param folder A folder containing the files
#' @return A character vector with four paths

get_ihme_files <- function(input_folder){
   files <- c("IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2000_Y2017M09D25.TIF",
              "IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2005_Y2017M09D25.TIF",
              "IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2010_Y2017M09D25.TIF",
              "IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2015_Y2017M09D25.TIF")
   file.path(input_folder, "imr_ihme", "data", files)
}
