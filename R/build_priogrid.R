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

<<<<<<< HEAD
static_pg <- function(as_raster = FALSE, test = FALSE){
=======
#' Collects and returns static (non-timevarying) PRIO-GRID data
#'
#' See [pgvariables] for an overview of variables.
#'
#' @param as_raster Boolean (default FALSE). Set to TRUE to return a list of rasters. Defaults to returning a data.table.
#' @param test Boolean. Prints coverage summary (percentage cells with data) for each variable.
#' @param overwrite Boolean (default FALSE). Checks if the data.table exists as a file if as_raster = FALSE.
#'  If it does, then it returns the data.table. If not, it saves the data.table to disk as .csv.gz and .parquet.
#'
#' @return a data.table with pgid as rows and variables as columns or a list of terra SpatRasters
#' @export
#'
#' @examples
#' \dontrun{
#'   pg_dt <- read_pg_static(test = TRUE) # prints coverage summary for each variable
#'   pg_rast <- read_pg_static(as_raster = TRUE)
#' }
read_pg_static <- function(as_raster = FALSE, test = FALSE, overwrite = FALSE){
  fname <- file.path(pgout_path(), "pg_static.parquet")
  if(as_raster == FALSE & test == FALSE & file.exists(fname) & !overwrite){
    return(arrow::read_parquet(fname))
  }

>>>>>>> 8f6f3af (Updated build step.)
  static <- pgvariables |> dplyr::filter(static)
  rasters <- lapply(static$name, load_pgvariable)
  names(rasters) <- static$name
  rasters <- rasters[!is.na(rasters)]

  if(test){
    pg_idx <- length(create_pg_indices())
    coverage_test <- dplyr::bind_rows(lapply(rasters, function(x) terra::freq(is.na(x))), .id = "variable") |>
      dplyr::filter(value == 0) |> # non-missing
      dplyr::mutate(coverage_pct = round(count / pg_idx, 4)) |>
      dplyr::select(variable, coverage_n = count, coverage_pct)
    message("# ---- Testing variable coverage ---- #")
    message(paste(capture.output(print(coverage_test, nrows = 50)), collapse = "\n"))
  }


  if(as_raster){
    return(rasters)
  }

  if(as_raster == FALSE & file.exists(fname) & !overwrite){
    return(arrow::read_parquet(fname))
  }

  df <- rast_to_df(terra::rast(rasters), static = TRUE)

  data.table::fwrite(df, paste0(tools::file_path_sans_ext(fname), ".csv.gz"), compress = "gzip")
  arrow::write_parquet(df, fname)

  return(df)
}


#' Collects and returns timevarying (non-static) PRIO-GRID data
#'
#' See [pgvariables] for an overview of variables.
#'
#' @param as_raster Boolean. Set to TRUE to return a list of rasters. Defaults to FALSE (returns data.table).
#' @param test Boolean. Returns only coverage summary (percentage cells with data) for each variable for each date.
#' @param overwrite Boolean (default FALSE). Checks if the data.table exists as a file if as_raster = FALSE.
#'  If it does, then it returns the data.table. If not, it saves the data.table to disk as .csv.gz and .parquet.
#'
#' @return a data.table with pgid + measurement_date as rows and variables as columns or a list of terra SpatRasters with each measurement date as layers or coverage test
#' @export
#'
#' @examples
#' \dontrun{
#'   pg_dt <- read_pg_timevarying() # data.table
#'   coverage_test <- read_pg_timevarying(test = TRUE) # coverage test data frame
#'   pg_rast <- read_pg_timevarying(as_raster = TRUE) # list of rasters
#' }
read_pg_timevarying <- function(as_raster = FALSE, test = FALSE, overwrite = FALSE){
  fname <- file.path(pgout_path(), "pg_timevarying.parquet")
  if(as_raster == FALSE & test == FALSE & file.exists(fname) & !overwrite){
    return(arrow::read_parquet(fname))
  }

  timevarying <- pgvariables |> dplyr::filter(!static)

  rasters <- lapply(timevarying$name, load_pgvariable)
  names(rasters) <- timevarying$name
  rasters <- rasters[!is.na(rasters)]

  if(test){
    pg_idx <- length(create_pg_indices())
    coverage_test <- dplyr::bind_rows(lapply(rasters, function(x) terra::freq(is.na(x))), .id = "variable") |>
      dplyr::filter(value == 0) |> # non-missing
      dplyr::mutate(coverage_pct = round(count / pg_idx, 4),
                    measurement_date = lapply(rasters, names) |> unlist()) |>
      dplyr::select(variable, measurement_date, coverage_n = count, coverage_pct)

    return(coverage_test)
  }

  if(as_raster == FALSE & file.exists(fname) & !overwrite){
    return(arrow::read_parquet(fname))
  }

  if(as_raster){
    return(rasters)
  }

  timevarying_lst <- mapply(rast_to_df, rasters, names(rasters), static = FALSE, SIMPLIFY = FALSE)

  # Only return dates that are within the data scope
  min_date <- do.call(min, lapply(timevarying_lst, function(x) min(x$measurement_date)))
  #max_date <- do.call(max, lapply(timevarying_lst, function(x) max(x$measurement_date)))
  my_dates <- pg_dates()
  my_dates <- my_dates[my_dates >= min_date]

  df <- expand.grid(pgid = create_pg_indices(), measurement_date = my_dates)
  df <- data.table::setDT(df, key = c("pgid", "measurement_date"))

  for(sdt in timevarying_lst) {
    df <- merge(df, sdt, all.x = TRUE)
  }

  data.table::fwrite(df, paste0(tools::file_path_sans_ext(fname), ".csv.gz"), compress = "gzip")
  arrow::write_parquet(df, fname)
  return(df)
}

build_priogrid_05deg_yearly <- function(){
  pgoptions$set_nrow(360)
  pgoptions$set_ncol(720)
  pgoptions$set_crs("epsg:4326")
  pgoptions$set_extent(c("xmin" = -180, "xmax" =  180, "ymin" = -90, "ymax" = 90))
  pgoptions$set_temporal_resolution("1 year")
  pgoptions$set_start_date(as.Date("1850-12-31"))
  pgoptions$set_end_date(as.Date("2025-08-26"))

  calc_pg(overwrite = TRUE)

  pg_t <- timevarying_pg(overwrite = TRUE)
  pg_s <- static_pg(overwrite = TRUE)

  persistent_path <- file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"), "05deg_yearly")
  dir.create(persistent_path)
  file.copy(from = list.files(pgout_path(), full = TRUE), to = persistent_path, copy.mode = TRUE, copy.date = FALSE)

  oldwd <- getwd()
  setwd(file.path(pgoptions$get_rawfolder(), "priogrid"))
  zip(paste("priogrid", packageVersion("priogrid"), "05deg_yearly.zip", sep = "_"),
      files = file.path(packageVersion("priogrid"), "05deg_yearly"), flags = "-r")
  setwd(oldwd)
}
