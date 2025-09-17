get_spatial_hash <- function() {
  spatial_options <- list(
    nrow = pgoptions$get_nrow(),
    ncol = pgoptions$get_ncol(),
    crs = pgoptions$get_crs(unparsed = TRUE),
    extent = as.vector(pgoptions$get_extent())
  )

  hash_input <- paste(spatial_options, collapse = "_")
  digest::digest(hash_input, algo = "md5") |> substr(1,6)
}

get_temporal_hash <- function() {
  # Avoid building new hash for small changes in end date.
  # Round to first date given temporal resolution.
  rounded_end_date <- lubridate::floor_date(
    pgoptions$get_end_date(),
    pgoptions$get_temporal_resolution()
  )

  temporal_options <- list(
    temporal_resolution = pgoptions$get_temporal_resolution(),
    start_date = pgoptions$get_start_date(),
    end_date = rounded_end_date
  )

  hash_input <- paste(temporal_options, collapse = "_")
  digest::digest(hash_input, algo = "md5") |> substr(1,6)
}


#' Path to store PRIOGRID results
#'
#' The path varies depending on your [pgoptions]:
#'  rawfolder/priogrid/(package version)/(spatial options hash)/(temporal options hash)
#'
#' @returns File path
#' @export
#'
#' @examples
#' \dontrun{
#'   pgout_path()
#'   list.files(pgout_path())
#' }
pgout_path <- function(){
  file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"), get_spatial_hash(), get_temporal_hash())
}

#' Calculates all PRIO-GRID variables
#'
#' In PRIO-GRID, all functions that are named "gen_something()" are functions that
#' return final variables as a SpatRaster (or a stack of these). This function
#' calculates all variables and store them in "path/to/your/rawfolder/priogrid/version/{options_hash}/variable_name.rds".
#'
#' @param varnames Vector with names of variables in PRIOGRID. See [pgvariables]. If NULL, then all variables are used.
#' @param overwrite Boolean, if false, will ignore updating variables that already exist in the result folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   calc_pg("ne_disputed_area_share")
#'   r <- load_pgvariable()
#' }
#'
calc_pg <- function(varnames = NULL, overwrite = FALSE){
  if(is.null(varnames)){
    varnames <- pgvariables$name
  }

  valid_varnames <- varnames[varnames %in% pgvariables$name]
  invalid_varnames <- varnames[!varnames %in% pgvariables$name]
  if(length(invalid_varnames)> 0){
    message(paste("Ignored varnames:", paste(invalid_varnames, collapse = ", ")))
  }

  if(length(valid_varnames) == 0){
    stop("No valid varnames supplied. Use names in `pgvariables`")
  }

  if(!dir.exists(pgout_path())){
    dir.create(pgout_path(), recursive = T)
  }

  existing_files <- list.files(pgout_path()) |> tools::file_path_sans_ext()

  if(!overwrite){
    valid_varnames <- valid_varnames[!valid_varnames %in% existing_files]
  }

  # Calculate variable
  message(paste("Variables to calculate:", paste(valid_varnames, collapse = ", ")))
  message(paste("Saving to:", pgout_path()))
  for(varname in valid_varnames){
    message(paste("Calculating", varname, "and saving to", pgout_path()))
    r <- get(paste0("gen_", varname))()
    save_pgvariable(r, varname)
  }
}

#' Save a PRIO-GRID variable
#'
#' Save a PRIO-GRID variable to a standard folder depending on your pgoptions:
#'  rawfolder/priogrid/(package version)/(spatial options hash)/(temporal options hash)
#'
#' Correctly wrap the terra object before writing it, and checks if the variable
#' is included in the list of variables in PRIOGRID [pgvariables].
#'
#' @param rast Terra SpatRast from a gen_ function
#' @param varname Character string with the variable name
#' @export
save_pgvariable <- function(rast, varname) {

  if(!varname %in% pgvariables$name){
    stop("varname not found in pgvariables.")
  }

  filepath <- file.path(pgout_path(), paste0(varname, ".rds"))

  if(class(rast) != "PackedSpatVector"){
    rast <- terra::wrap(rast)
  }

  saveRDS(rast, filepath)
}

#' Load a PRIO-GRID variable
#'
#' This points to different folders depending on your pgoptions:
#'  rawfolder/priogrid/(package version)/(spatial options hash)/(temporal options hash)
#'
#'
#' @param varname Character string with the variable name
#' @return A SpatRaster object
#' @export
load_pgvariable <- function(varname) {
  filepath <- file.path(pgout_path(), paste0(varname, ".rds"))

  if (!file.exists(filepath)) {
    return(NA)
  }

  r_wrapped <- readRDS(filepath)
  terra::unwrap(r_wrapped)
}

static_pg <- function(as_raster = FALSE, test = FALSE){
  static <- pgvariables |> dplyr::filter(static)
  rasters <- lapply(static$name, load_pgvariable)
  names(rasters) <- static$name
  rasters <- rasters[!is.na(rasters)]

  if(as_raster){
    return(rasters)
  }

  df <- rast_to_df(terra::rast(rasters), static = TRUE)

  if(test){
    cols <- setdiff(names(df), "pgid")
    pg_idx <- length(create_pg_indices())

    coverage_test <- df |>
      dplyr::summarise(dplyr::across(dplyr::all_of(cols), ~ sum(!is.na(.x)))) |>
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "pgid_cov") |>
      dplyr::mutate(pgid_cov_pct = pgid_cov / pg_idx)


    message("# ---- Testing variable coverage ---- #")
    message(paste(capture.output(print(coverage_test, nrows = 50)), collapse = "\n"))
  }

  return(df)
}



timevarying_pg <- function(as_raster = FALSE, test = FALSE){
  timevarying <- pgvariables |> dplyr::filter(!static)

  rasters <- lapply(timevarying$name, load_pgvariable)
  names(rasters) <- timevarying$name
  rasters <- rasters[!is.na(rasters)]

  if(as_raster){
    return(rasters)
  }

  timevarying_lst <- mapply(rast_to_df, rasters, names(rasters), static = FALSE, SIMPLIFY = FALSE)

  # Only return dates that are within the data scope
  min_date <- do.call(min, lapply(timevarying_lst, function(x) min(x$measurement_date)))
  #max_date <- do.call(max, lapply(timevarying_lst, function(x) max(x$measurement_date)))
  my_dates <- pg_dates()
  my_dates <- my_dates[my_dates >= min_date]

  if(test){
    pg_idx <- create_pg_indices()
    date_coverage <- sapply(timevarying_lst, function(x) length(unique(x$measurement_date) %in% my_dates))
    pgid_coverage <- sapply(timevarying_lst, function(x) length(unique(x$pgid) %in% pg_idx))
    coverage_test <- data.table::as.data.table(cbind(date_coverage, pgid_coverage), keep.rownames = "variable")
    coverage_test[, `:=`(date_cov_pct = date_coverage/length(my_dates),
                         pgid_cov_pct = pgid_coverage/length(pg_idx))]

    message("# ---- Testing variable coverage ---- #")
    message(paste(capture.output(print(coverage_test, nrow = 50)), collapse = "\n"))
  }

  df <- expand.grid(pgid = create_pg_indices(), measurement_date = my_dates)
  df <- data.table::setDT(df, key = c("pgid", "measurement_date"))

  for(sdt in timevarying_lst) {
    df <- sdt[df, on = .(pgid, measurement_date)]
  }
  return(df)
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
