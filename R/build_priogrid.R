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
#' @param version Default NULL, will pull version using `packageVersion("priogrid")`. Alternative a character string to specify a PRIO-GRID version.
#' @param type Default NULL, will build temporal and spatial hash based on pgoptions. Alternative "05deg_yearly" to access the published version.
#'
#' @returns File path
#' @export
#'
#' @examples
#' \dontrun{
#'   pgout_path() # to get path for when you develop data yourself
#'   list.files(pgout_path())
#'   pgout_path(version = "3.0.0", type = "05deg_yearly") # published version
#' }
pgout_path <- function(version = NULL, type = NULL){
  if(length(c(is.null(version), is.null(type))) == 1){
    stop("Please specify both PRIOGRID version and type, or let both be NULL to infer from pgoptions")
  }

  if(is.null(version) & is.null(type)){
    fpath <- file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"), get_spatial_hash(), get_temporal_hash())
  } else{
    fpath <- file.path(pgoptions$get_rawfolder(), "priogrid", version, type)
  }
  return(fpath)
}

#' Calculates all PRIO-GRID variables
#'
#' In PRIO-GRID, all functions that are named "gen_something()" are functions that
#' return final variables as a SpatRaster (or a stack of these). This function
#' calculates all variables and store them in "path/to/your/rawfolder/priogrid/version/{options_hash}/variable_name.rds".
#'
#' @param varnames Vector with names of variables in PRIOGRID. See [pgvariables]. If NULL, then all variables are used.
#' @param type PRIOGRID type to [pgout_path]. Use "hash" to make folder based on pgoptions.
#' @param version PRIOGRID version to [pgout_path]. Defaults to `packageVersion("priogrid")`
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
calc_pg <- function(varnames = NULL, type, version = packageVersion("priogrid"), overwrite = FALSE){
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

  if(!dir.exists(pgout_path(version, type))){
    dir.create(pgout_path(version, type), recursive = T)
  }

  existing_files <- list.files(pgout_path(version, type)) |> tools::file_path_sans_ext()

  if(!overwrite){
    valid_varnames <- valid_varnames[!valid_varnames %in% existing_files]
  }

  # Calculate variable
  message(paste("Variables to calculate:", paste(valid_varnames, collapse = ", ")))
  message(paste("Saving to:", pgout_path(version, type)))
  for(varname in valid_varnames){
    message(paste("Calculating", varname, "and saving to", pgout_path(version, type)))
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
#' @param type PRIOGRID type to [pgout_path]. Use "hash" to make folder based on pgoptions.
#' @param version PRIOGRID version to [pgout_path]. Defaults to `packageVersion("priogrid")`
#' @export
save_pgvariable <- function(rast, varname, type, version = packageVersion("priogrid")) {

  if(!varname %in% pgvariables$name){
    stop("varname not found in pgvariables.")
  }

  filepath <- file.path(pgout_path(version = version, type = type), paste0(varname, ".rds"))

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
#' @param version PRIOGRID version to [pgout_path]. Default (NULL) targets the latest PRIOGRID version.
#' @param type PRIOGRID type to [pgout_path]. Default (NULL) targets the yearly 0.5x0.5 degree spatio-temporal resolution.
#' @return A SpatRaster object
#' @export
load_pgvariable <- function(varname, version = NULL, type = NULL) {
  filepath <- file.path(pgout_path(version = version, type = type), paste0(varname, ".rds"))

  if (!file.exists(filepath)) {
    return(NA)
  }

  r_wrapped <- readRDS(filepath)
  terra::unwrap(r_wrapped)
}

#' Collects and returns static (non-timevarying) PRIO-GRID data
#'
#' See [pgvariables] for an overview of variables.
#'
#' @param version PRIOGRID version to [pgout_path]. Default (NULL) targets the latest PRIOGRID version.
#' @param type PRIOGRID type to [pgout_path]. Default (NULL) targets the yearly 0.5x0.5 degree spatio-temporal resolution.
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
read_pg_static <- function(version = NULL, type = NULL, as_raster = FALSE, test = FALSE, overwrite = FALSE){
  fname <- file.path(pgout_path(version, type), "pg_static.parquet")
  if(as_raster == FALSE & test == FALSE & file.exists(fname) & !overwrite){
    return(arrow::read_parquet(fname))
  }
  static <- pgvariables |> dplyr::filter(static)
  rasters <- lapply(static$name, load_pgvariable, version = version, pgtype = pgtype)
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
#' @param version PRIOGRID version to [pgout_path]. Default (NULL) targets the latest PRIOGRID version.
#' @param type PRIOGRID type to [pgout_path]. Default (NULL) targets the yearly 0.5x0.5 degree spatio-temporal resolution.
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
read_pg_timevarying <- function(version = NULL, type = NULL,
                                as_raster = FALSE, test = FALSE, overwrite = FALSE){
  fname <- file.path(pgout_path(version, type), "pg_timevarying.parquet")
  if(as_raster == FALSE & test == FALSE & file.exists(fname) & !overwrite){
    return(arrow::read_parquet(fname))
  }

  timevarying <- pgvariables |> dplyr::filter(!static)

  rasters <- lapply(timevarying$name, load_pgvariable, version = version, pgtype = pgtype)
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

  version <- "3.0.0"
  sanitized_version <- stringr::str_replace_all(version, "\\.", "_")
  type <- "05deg_yearly"

  calc_pg(overwrite = TRUE)

  pg_t <- timevarying_pg(overwrite = TRUE)
  pg_s <- static_pg(overwrite = TRUE)

  persistent_path <- file.path(pgoptions$get_rawfolder(), "priogrid", version, type)
  dir.create(persistent_path)
  file.copy(from = list.files(pgout_path(version, type), full = TRUE), to = persistent_path, copy.mode = TRUE, copy.date = FALSE)


  oldwd <- getwd()
  setwd(file.path(pgoptions$get_rawfolder(), "priogrid"))
  zip(paste0(paste("priogrid", sanitized_version, type, sep = "_"), ".zip"),
      files = file.path(version, type), flags = "-r")
  setwd(oldwd)
}

initialize_priogrid <- function(version = "3.0.0", type = "05deg_yearly", overwrite = FALSE){
  pgdf <- tibble::tibble_row(
    version = "3.0.0",
    pgtype = "05deg_yearly",
    url = "https://cdn.cloud.prio.org/files/379b7254-b47c-48f3-a650-783348d0ff7e",
    fname = "priogrid_3_0_0_05deg_yearly.zip"
  )

  current <- pgdf |> dplyr::filter(version == (!!version), pgtype == (!!pgtype))

  fpath <- file.path(pgoptions$get_rawfolder(), "priogrid", current$fname)

  if(!file.exists(fpath) | overwrite){
    curl::multi_download(current$url,
                         destfiles = fpath,
                         resume = TRUE)
  }

  pgfiles <- unzip(fpath, list = TRUE)
  if(!all(file.exists(file.path(pgoptions$get_rawfolder(), "priogrid", pgfiles$Name)))){
    unzip(fpath, exdir = file.path(pgoptions$get_rawfolder(), "priogrid"))
  }
}
