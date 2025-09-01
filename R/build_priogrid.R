#' Get a hash of options that affect data generation
get_options_hash <- function() {
  # Get the options that actually affect the output
  relevant_options <- list(
    nrow = pgoptions$get_nrow(),
    ncol = pgoptions$get_ncol(),
    crs = pgoptions$get_crs(),
    extent = as.vector(pgoptions$get_extent()),
    temporal_resolution = pgoptions$get_temporal_resolution(),
    start_date = pgoptions$get_start_date(),
    end_date = pgoptions$get_end_date()
  )

  # Create a short hash
  hash_input <- paste(relevant_options, collapse = "_")
  digest::digest(hash_input, algo = "md5")
}


#' Calculates all PRIO-GRID variables
#'
#' In PRIO-GRID, all functions that are named "gen_something()" are functions that
#' return final variables as a SpatRaster (or a stack of these). This function
#' calculates all variables and store them in "path/to/your/rawfolder/priogrid/version/{options_hash}/variable_name.rds".
#'
#' @param gen_functions Character vector with priogrid functions starting with gen_. If NULL, then all are used.
#' @param overwrite Boolean, if false, will ignore updating variables that already exist in the result folder
#'
#' @export
#'
#' @examples
#' calculate_pgvariables()
#' r <- readRDS(file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"), "naturalearth_cover.rds"))
calculate_pgvariables <- function(gen_functions = NULL, overwrite = FALSE){
  pg_functions <- ls("package:priogrid")
  if(is.null(gen_functions)){

    gen_functions <- pg_functions[grepl("gen_", pg_functions)]
  } else{
    assertthat::assert_that(all(gen_functions %in% pg_functions), msg = "Some functions provided does not exist.")
  }

  # Include options hash in the path
  options_hash <- get_options_hash()
  priogrid_outpath <- file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"), options_hash)

  if(!dir.exists(priogrid_outpath)){
    dir.create(priogrid_outpath, recursive = T)
  }

  varnames <- stringr::str_remove(gen_functions, "gen_")
  existing_files <- list.files(priogrid_outpath) |> tools::file_path_sans_ext()

  if(!overwrite){
    ignore_these <- varnames[varnames %in% existing_files]
  } else{
    ignore_these <- c("")
  }

  for(f in gen_functions){
    varname <- stringr::str_remove(f, "gen_")
    if(varname %in% ignore_these){
      next()
    }

    fname <- paste0(varname, ".rds")
    r <- get(f)()
    r_wrapped <- terra::wrap(r)
    saveRDS(r_wrapped, file.path(priogrid_outpath, fname))
  }
}

#' Load a PRIO-GRID variable
#'
#' @param varname Character string with the variable name
#' @return A SpatRaster object
#' @export
load_pgvariable <- function(varname) {
  options_hash <- get_options_hash()
  priogrid_outpath <- file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"), options_hash)
  fname <- paste0(varname, ".rds")
  filepath <- file.path(priogrid_outpath, fname)

  if (!file.exists(filepath)) {
    return(NA)
  }

  r_wrapped <- readRDS(filepath)
  terra::unwrap(r_wrapped)
}

#' Collects PRIO-GRID data from rasters and pulls it into two data.frames
#'
#' One dataframe is called "static", with data without any time-varying dimension. The "timevarying"
#' dataframe provides data from pgoptions$get_start_date() to pgoptions$get_end_date() with
#' pgoptions$get_temporal_resolution() for all variables with a time-varying dimension.
#' Cells and time-periods without any non-missing data are dropped.
#'
#' @return a list with two data.frames
#' @export
#'
#' @examples
#' \donotrun{
#'   pg <- collate_pgdata()
#'   nrow(pg$static)
#'   nrow(pg$timevarying)
#' }

collate_pgdata <- function(return_raster = FALSE){
  static_variables <- pgvariables |> dplyr::filter(static)
  nonstatic_variables <- pgvariables |> dplyr::filter(!static)


  variables <- lapply(pgvariables$name, load_pgvariable)
  names(variables) <- pgvariables$name

  variables <- variables[!is.na(variables)]

  static <- variables[names(variables) %in% pgvariables$name[pgvariables$static]]
  timevarying <- variables[names(variables) %in% pgvariables$name[!pgvariables$static]]

  if(return_raster){
    return(list("static" = static, "timevarying" = timevarying))
  }

  static_df <- rast_to_df(terra::rast(static), static = TRUE)
  timevarying_lst <- mapply(rast_to_df, timevarying, names(timevarying), static = FALSE, SIMPLIFY = FALSE)

  timevarying_df <- expand.grid(pgid = create_pg_indices(), measurement_date = pg_dates())

  for(df in timevarying_lst){
    timevarying_df <- dplyr::left_join(timevarying_df, df, by = c("pgid", "measurement_date"))
  }

  timevarying_df <- timevarying_df[rowSums(!is.na(timevarying_df)) > 2,] # remove rows with only missing elements.
  timevarying_df <- timevarying_df |> dplyr::arrange(pgid, measurement_date)

  return(list("static" = static_df, "timevarying" = timevarying_df))
}

build_priogrid_default <- function(){
  pgoptions$set_nrow(360)
  pgoptions$set_ncol(720)
  pgoptions$set_crs("epsg:4326")
  pgoptions$set_extent(c("xmin" = -180, "xmax" =  180, "ymin" = -90, "ymax" = 90))
  pgoptions$set_temporal_resolution("1 year")
  pgoptions$set_start_date(as.Date("1850-12-31"))
  pgoptions$set_end_date(as.Date("2025-08-26"))

  calculate_pgvariables(overwrite = TRUE)

  pg <- collate_pgdata()

}
