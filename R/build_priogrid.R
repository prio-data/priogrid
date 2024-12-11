#' Calculates all PRIO-GRID variables
#'
#' In PRIO-GRID, all functions that are named "gen_something()" are functions that
#' return final variables as a SpatRaster (or a stack of these). This function
#' calculates all variables and store them in "path/to/your/rawfolder/priogrid/version/variable_name.rds".
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

  priogrid_outpath <- file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"))

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
    saveRDS(r, file.path(priogrid_outpath, fname))
  }
}

#' Collects PRIO-GRID data from rasters and pulls it into two data.frames
#'
#' One dataframe is called "static", with data without any time-varying dimension. The "non_static"
#' dataframe provides data from pgoptions$get_start_date() to pgoptions$get_end_date() with
#' pgoptions$get_temporal_resolution() for all variables with a time-varying dimension.
#'
#' @return a list with two data.frames
#' @export
#'
#' @examples
#' pg <- collate_pgdata()
#' nrow(pg$static)
#' nrow(pg$non_static)
collate_pgdata <- function(){
  priogrid_outpath <- file.path(pgoptions$get_rawfolder(), "priogrid", packageVersion("priogrid"))
  variables_files <- list.files(priogrid_outpath)
  variables <- pgvariables |> dplyr::filter(name %in% (variables_files |> tools::file_path_sans_ext()))
  variables$fname <- list.files(priogrid_outpath, full.names = T)


  non_static <- expand.grid(pgid = create_pg_indices(), measurement_date = pg_dates())
  static <- dplyr::tibble()
  for(i in 1:nrow(variables)){
    r <- readRDS(variables$fname[i])
    if(class(r) == "PackedSpatRaster"){
      r <- terra::unwrap(r)
    }
    if(variables$static[i]){
      df <- rast_to_df(r, static = TRUE)
      static <- dplyr::bind_rows(static, df)
    } else{
      df <- rast_to_df(r, static = FALSE, varname = variables$name[i]) |> na.omit()
      df <- df |> dplyr::mutate(measurement_date = as.Date(measurement_date))
      non_static <- dplyr::left_join(non_static, df, by = c("pgid", "measurement_date"))
    }
  }
  return(list("static" = static, "non_static" = non_static))
}
