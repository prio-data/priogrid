#' Search PRIO-GRID meta-data
#'
#' Use regex to search the meta-data for the data you are interested in.
#'
#' @param search_string A character string to search in source name, source version, source id, source tags, spatial extent, temporal
#' resolution, or in bibliography elements.
#' @param bib_element Supports author, journal, year, or title. If null, the search
#' will not search bibliography elements.
#'
#' @return list with data.frames
#' @export
#'
#' @examples
#' pgsearch("GHSL")
pgsearch <- function(search_string, bib_element = NULL){
  in_name <- pgsources |> dplyr::filter(grepl(search_string, source_name, ignore.case = T))
  in_version <- pgsources |> dplyr::filter(grepl(search_string, source_version, ignore.case = T))
  in_id <- pgsources |> dplyr::filter(grepl(search_string, id, ignore.case = T))
  in_tags <- pgsources |> dplyr::filter(grepl(search_string, tags, ignore.case = T))
  in_spatial_extent <- pgsources |> dplyr::filter(grepl(search_string, spatial_extent, ignore.case = T))
  in_temporal_resolution <- pgsources |> dplyr::filter(grepl(search_string, temporal_resolution, ignore.case = T))

  if(is.null(bib_element)){
    return(list("in_name" = in_name, "in_version" = in_version, "in_id" = in_id,
                "in_tags" = in_tags, "in_spatial_extent" = in_spatial_extent, "in_temporal_resolution" = in_temporal_resolution))
  } else{
    in_bib_element <- pgsources |>
      dplyr::rowwise() |>
      dplyr::mutate(bib_element = list(extract_bib_elements(citation_keys) |> unlist())) |>
      dplyr::filter(grepl(search_string, bib_element, ignore.case = T) |> any())

    return(list("in_name" = in_name, "in_version" = in_version, "in_id" = in_id,
                "in_tags" = in_tags, "in_spatial_extent" = in_spatial_extent, "in_temporal_resolution" = in_temporal_resolution,
                "in_element" = in_bib_element))
  }
}

#' Extract url- and file-info from PRIO-GRID metadata
#'
#' @param use_mirror Boolean. Whether or not to use PRIO-GRID mirror.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' file_info <- pg_rawfiles()
pg_rawfiles <- function(use_mirror = TRUE, only_file_extensions = FALSE){
  if(use_mirror){
    urls <- pgsources |>
      dplyr::mutate(url = dplyr::if_else(is.na(prio_mirror), download_url, prio_mirror)) |>
      dplyr::select(id, source_name, source_version, url)
  } else{
    urls <- pgsources |> dplyr::select(id, source_name, source_version, url = download_url)
  }

  urls <- urls[!is.na(urls$url),]

  parse_source_url <- function(url){
    if(grepl("urls/", url) |> all()){
      url <- readLines(system.file("extdata", url, package = "priogrid"))
    }
    return(list(url))
  }

  urls$url_list <- sapply(urls$url, parse_source_url)

  # Unnest because url-column can be a list of urls
  urls <- tidyr::unnest(urls, cols = c(url_list)) |> dplyr::select(source_name, source_version, id, url = url_list)
  urls <- urls |> dplyr::mutate(filename = basename(url)  |> stringr::str_remove("\\?.*"))

  if(only_file_extensions){
    # For testing
    return(tools::file_ext(urls$filename))
  }
  urls <- urls |> dplyr::mutate(filename = file.path(source_name, source_version, id, filename))
  return(urls)
}

#' Test if MD5 checksums of local files are the same as a tested set of files
#'
#' Here, we use [pgchecksum], which we created when testing
#' PRIO-GRID, and test it against a similar method for your own
#' local files. This is to verify that you are using the same files
#' as we used to build PRIO-GRID.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' res <- check_pgsourcefiles()
check_pgsourcefiles <- function(){
  destfolder <- pgoptions$get_rawfolder()
  file_info <- pg_rawfiles()

  lacking_pgchecksum <- dplyr::anti_join(file_info, pgchecksum, by = c("source_name", "source_version", "id", "filename"))

  if(nrow(lacking_pgchecksum)>0){
    stop("pgchecksum data is outdated. Please contact the administrators of PRIO-GRID.")
  }

  local_checksum <- file_info |> dplyr::mutate(
    local_md5 = tools::md5sum(file.path(destfolder, filename))
  ) |> dplyr::select(source_name, source_version, id, filename, local_md5)

  df <- dplyr::left_join(local_checksum, pgchecksum, by = c("source_name", "source_version", "id", "filename")) |>
    dplyr::mutate(files_are_equal = local_md5 == md5)

  if(all(df$files_are_equal)){
    print("All files in your local storage are similar to a tested set.")
  } else{
    print("Some files in your local storage are different to a tested set. Please see the returned data.frame for details.")
  }
  return(df)
}

#' Get file-path on local system to a data source in PRIO-GRID
#'
#' To look up src_name and version in PRIO-GRID, see [pgmeta] or [pg_rawfiles()].
#'
#' @param src_name character, the source name
#' @param version character, the version number
#'
#' @return file path, string
#' @export
#'
#' @examples
#' get_pgfile(source_name = "ETH ICR cShapes", source_version = "2.0", id = "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
get_pgfile <- function(source_name, source_version, id){
  f <- pg_rawfiles() |> dplyr::filter(source_name == !!rlang::enquo(source_name),
                                      source_version == !!rlang::enquo(source_version),
                                      id == !!rlang::enquo(id)) |> dplyr::pull(filename)
  destfolder <- pgoptions$get_rawfolder()
  if(length(f) == 0){
    return(message("No files in metadata with that name and version."))
  }

  if(!dir.exists(destfolder)){
    stop(paste(destfolder, "does not exist. Please pgoptions$set_rawfolder()."))
  }

  full_file_path <- file.path(destfolder, f)

  file_found <- file.exists(full_file_path)
  if(!all(file_found)){
    stop(paste("Some files were not found in", destfolder, ":\n", f[!file_found], "\n"))
  }

  return(full_file_path)
}

#' Download the raw-data for PRIO-GRID
#'
#' Before running this, you need to set the folder using pgoptions$set_rawfolder("path/to/folder")
#'
#' @param overwrite Whether or not to download and overwrite files already in local folder.
#' @param file_info A data.frame with the same structure as the result from [pg_rawfiles()]. If file_info is null (default),
#'   then file_info will be all data returned from [pg_rawfiles()].
#' @param resume If true, will also download files that did not finish download last time the function was run.
#'
#' @return data.frame Download summary
#' @export
#'
#' @examples
#' files_to_download <- pg_rawfiles() |> dplyr::filter(id == "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
#' # download_pg_rawdata(overwrite = TRUE, file_info = files_to_download)
download_pg_rawdata <- function(file_info = NULL, overwrite = FALSE, batch_size = 20, max_retry = 10){
  destfolder <- pgoptions$get_rawfolder()

  if(!dir.exists(destfolder)){
    accept <- readline(paste("Destination folder", destfolder, "does not exist. Do you want to create? (Y)es: "))
    if(accept == "Y" | accept == "Yes"){
      dir.create(destfolder)
    } else(
      return(message("No folder to store data."))
    )
  }

  if(is.null(file_info)){
      file_info <- pg_rawfiles()
  }

  file_info$file_exists <- file.exists(file.path(destfolder, file_info$filename))
  file_info$subdir_exists <- dir.exists(file.path(destfolder, dirname(file_info$filename)))


  if(!overwrite){
    file_info <- file_info |> dplyr::filter(!file_exists)
  }

  total_files <- nrow(file_info)
  if(total_files==0){
    return(message(paste("All raw-files are already in", destfolder)))
  }

  subdirs <- file_info |> dplyr::filter(!subdir_exists) |> dplyr::pull(filename) |> dirname() |> unique()
  if(length(subdirs) > 0){
    for(newdir in subdirs){
      dir.create(file.path(destfolder, newdir), recursive = TRUE)
    }
  }

  batch_download <- function(file_info, batch_size){
    nr <- nrow(file_info)
    file_batches <- split(file_info, rep(1:ceiling(nr/batch_size), each=batch_size, length.out=nr))


  unfinished_files <- batch_download(file_info, batch_size)

  if(nrow(unfinished_files) == 0){
    return()
  }

  for(i in 1:max_retry){
    warning("Download was interrupted before finished. Resuming.")
    unfinished_files <- batch_download(unfinished_files, batch_size)

    if(nrow(unfinished_files) == 0){
      return()
    }
