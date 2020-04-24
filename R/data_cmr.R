
#' @title cmr_mean
#'
#' @description Generate mean child malnutrition variable.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_cmr_mean <- function(input_folder){
   cmr_mean <- priogrid::gen_cmr(input_folder, fun = "mean")

   names(cmr_mean)[3] <- "cmr_mean"

   return(cmr_mean)
}


#' @title cmr_sd
#'
#' @description Generate standard deviation child malnutrition variable.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_cmr_sd <- function(input_folder){
   cmr_sd <- priogrid::gen_cmr(input_folder, fun = "sd")

   names(cmr_sd)[3] <- "cmr_sd"

   return(cmr_sd)
}

#' @title cmr_min
#'
#' @description Generate minimum child malnutrition variable.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_cmr_min <- function(input_folder){
   cmr_min <- priogrid::gen_cmr(input_folder, fun = "min")

   names(cmr_min)[3] <- "cmr_min"

   return(cmr_min)
}


#' @title cmr_max
#'
#' @description Generate maximum child malnutrition variable.
#'
#' @param input_folder Path to [pg-folder].
#'
#' @export

gen_cmr_max <- function(input_folder){
   cmr_max <- priogrid::gen_cmr(input_folder, fun = "max")

   names(cmr_max)[3] <- "cmr_max"

   return(cmr_max)
}


# Prep function
gen_cmr <- function(input_folder, fun){
   malnut <- raster::raster(file_path(input_folder, "cmr", "data", "uw.asc"))
   malnut <- malnut/10

   raster::extent(malnut) <- priogrid::prio_extent()

   malnut <- priogrid::raster_to_pg(malnut, aggregation_function = fun)

   malnut <- priogrid::rasterextent_to_pg(malnut)

   malnut <- priogrid::raster_to_tibble(malnut, add_pg_index = TRUE)

   return(malnut)
}

