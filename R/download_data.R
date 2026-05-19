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
#' @param only_file_extensions Logical. If TRUE, returns file extensions only. Used for testing. Default FALSE.
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
#' @param verbose Logical. If TRUE (default), prints a summary message.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' res <- check_pgsourcefiles()
check_pgsourcefiles <- function(verbose = TRUE){
  destfolder <- pg_rawfolder()
  file_info <- pg_rawfiles()

  lacking_pgchecksum <- dplyr::anti_join(file_info, pgchecksum, by = c("source_name", "source_version", "id", "filename"))

  if (nrow(lacking_pgchecksum) > 0) {
    warning(
      nrow(lacking_pgchecksum), " file(s) have no reference checksum in pgchecksum and will be skipped.\n",
      "  These are likely sources added after the last tested build.\n",
      "  Sources without checksums: ",
      paste(unique(lacking_pgchecksum$source_name), collapse = ", "), "\n",
      "  Run pg_update_checksums() to update the reference checksums.",
      call. = FALSE)
  }

  # Only check files present locally
  file_info$local_path <- file.path(destfolder, file_info$filename)
  present_files <- file_info[file.exists(file_info$local_path), ]

  if (nrow(present_files) == 0) {
    message("No local raw files found in: ", destfolder,
            "\nRun download_pg_rawdata() first.")
    return(invisible(NULL))
  }

  checkable <- dplyr::inner_join(
    present_files, pgchecksum, by = c("source_name", "source_version", "id", "filename")
  )

  if (nrow(checkable) == 0) {
    message("None of the locally present files have reference checksums in pgchecksum.")
    return(invisible(NULL))
  }

  checkable$local_md5 <- tools::md5sum(checkable$local_path)
  df <- dplyr::mutate(checkable, files_are_equal = local_md5 == md5) |>
    dplyr::select(source_name, source_version, id, filename, local_md5, md5, files_are_equal)

  n_ok <- sum(df$files_are_equal)
  n_bad <- sum(!df$files_are_equal)

  if (verbose) {
    if (n_bad == 0) {
      message("All ", n_ok, " checked file(s) match the tested reference checksums.")
    } else {
      message(
        n_ok, " file(s) match. ", n_bad, " file(s) differ from tested checksums:\n  ",
        paste(df$filename[!df$files_are_equal], collapse = "\n  "), "\n",
        "These may be updated versions or corrupted downloads.\n",
        "Re-download with: download_pg_rawdata(..., overwrite = TRUE)")
    }
  }
  return(invisible(df))
}

#' Get file-path on local system to a data source in PRIO-GRID
#'
#' To look up src_name and version in PRIO-GRID, see [pg_rawfiles()].
#'
#' @param source_name Character. The source name.
#' @param source_version Character. The version number.
#' @param id Character. The source id (UUID).
#' @param verify_checksums Logical. If TRUE, verifies file checksums against
#'   stored MD5 values. Defaults to value from [pg_current_config()].
#'
#' @return file path, string
#' @export
#'
#' @examples
#' get_pgfile(source_name = "ETH ICR cShapes", source_version = "2.0", id = "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
get_pgfile <- function(source_name, source_version, id,
                       verify_checksums = pg_current_config()$verify_checksums) {
  file_info <- pg_rawfiles() |> dplyr::filter(source_name == !!rlang::enquo(source_name),
                                      source_version == !!rlang::enquo(source_version),
                                      id == !!rlang::enquo(id))
  destfolder <- pg_rawfolder()

  if (length(file_info$filename) == 0) {
    stop(sprintf(
      "No files found in metadata for source_name='%s', source_version='%s', id='%s'.\n",
      source_name, source_version, id),
      "  Use pgsearch() or pg_rawfiles() to check available sources.",
      call. = FALSE)
  }

  if (!dir.exists(destfolder)) {
    stop(paste(destfolder, "does not exist. Please use pg_set_rawfolder()."))
  }

  full_file_path <- file.path(destfolder, file_info$filename)

  file_found <- file.exists(full_file_path)
  if (!all(file_found) & pg_current_config()$automatic_download) {
    download_pg_rawdata(file_info = file_info)
  }

  file_found <- file.exists(full_file_path)
  if (!all(file_found)) {
    missing_names <- file_info$filename[!file_found]
    if (pg_current_config()$automatic_download) {
      stop(sprintf(
        "%d file(s) for '%s' v%s not found after download attempt:\n  %s\n",
        sum(!file_found), source_name, source_version,
        paste(missing_names, collapse = "\n  ")),
        "  The download may have failed or been interrupted.\n",
        "  Try download_pg_rawdata() manually, or check pg_data_availability().",
        call. = FALSE)
    } else {
      stop(sprintf(
        "%d file(s) for '%s' v%s are missing from '%s':\n  %s\n",
        sum(!file_found), source_name, source_version, destfolder,
        paste(missing_names, collapse = "\n  ")),
        "  automatic_download is FALSE, so no download was attempted.\n",
        "  Run download_pg_rawdata() or set automatic_download=TRUE in pg_config().",
        call. = FALSE)
    }
  }

  if (isTRUE(verify_checksums)) {
    checkable <- dplyr::inner_join(file_info, pgchecksum,
                                   by = c("source_name", "source_version", "id", "filename"))
    if (nrow(checkable) > 0) {
      checkable$local_md5 <- tools::md5sum(file.path(destfolder, checkable$filename))
      mismatches <- checkable[checkable$local_md5 != checkable$md5, ]
      if (nrow(mismatches) > 0) {
        warning(nrow(mismatches), " file(s) for '", source_name,
                "' do not match tested checksums:\n  ",
                paste(mismatches$filename, collapse = "\n  "), "\n",
                "  Run check_pgsourcefiles() for details, or re-download with overwrite=TRUE.",
                call. = FALSE)
      }
    }
  }

  return(full_file_path)
}

#' Check which PRIO-GRID raw data files are available locally
#'
#' Returns a summary of which data sources have been downloaded to the raw data
#' folder. Useful for checking data status before running compute-heavy functions.
#'
#' @return A data.frame with columns `source_name`, `source_version`, `n_files`,
#'   `n_present`, and `all_present`, or NULL if the raw data folder is not set.
#' @export
#'
#' @examples
#' \dontrun{
#' pg_data_availability()
#' }
pg_data_availability <- function() {
  tryCatch({
    destfolder <- pg_rawfolder()
    fi <- pg_rawfiles()
    fi$file_exists <- file.exists(file.path(destfolder, fi$filename))
    fi |>
      dplyr::group_by(source_name, source_version) |>
      dplyr::summarise(
        n_files = dplyr::n(),
        n_present = sum(file_exists),
        all_present = all(file_exists),
        .groups = "drop"
      )
  }, error = function(e) {
    message("Raw data folder not set. Use pg_set_rawfolder() to configure.")
    invisible(NULL)
  })
}

#' Download the raw-data for PRIO-GRID
#'
#' Before running this, you need to set the folder using pg_set_rawfolder("path/to/folder")
#'
#' @param file_info A data.frame with the same structure as the result from [pg_rawfiles()]. If file_info is null (default),
#'   then file_info will be all data returned from [pg_rawfiles()].
#' @param overwrite Whether or not to download and overwrite files already in local folder.
#' @param batch_size Integer. Number of files per download batch. Default 20.
#' @param max_retry Integer. Maximum number of retry attempts for failed downloads. Default 10.
#'
#' @return data.frame Download summary
#' @export
#'
#' @examples
#' files_to_download <- pg_rawfiles() |> dplyr::filter(id == "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
#' # download_pg_rawdata(overwrite = TRUE, file_info = files_to_download)
download_pg_rawdata <- function(file_info = NULL, overwrite = FALSE, batch_size = 20, max_retry = 10){
  destfolder <- pg_rawfolder()

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

    download_reports <- list()
    for(i in 1:length(file_batches)){
      message(paste("Processing batch:", i, "/", length(file_batches)))
      batch <- file_batches[[i]]
      download_reports[[i]] <- curl::multi_download(batch$url, file.path(destfolder, batch$filename), resume = TRUE)
    }

    download_report <- dplyr::bind_rows(download_reports) |> dplyr::filter(!(success %in% c(TRUE))) # NA or FALSE

    if(nrow(download_report) == 0){
      return(file_info[0, ])
    }

    unfinished_files <- file_info[file_info$url %in% download_report$url,]

    return(unfinished_files)
  }

  unfinished_files <- batch_download(file_info, batch_size)

  for(i in seq_len(max_retry)){
    if(nrow(unfinished_files) == 0) break
    warning("Download was interrupted before finished. Resuming.", call. = FALSE)
    unfinished_files <- batch_download(unfinished_files, batch_size)
  }

  if (nrow(unfinished_files) > 0) {
    message(nrow(unfinished_files), " file(s) could not be downloaded after ", max_retry,
            " retries: ", paste(unfinished_files$filename, collapse = ", "))
  }

  # Verify MD5 of newly downloaded files against pgchecksum where available
  successful_files <- file_info[!file_info$url %in% unfinished_files$url, ]
  checkable <- dplyr::inner_join(successful_files, pgchecksum,
                                 by = c("source_name", "source_version", "id", "filename"))
  if (nrow(checkable) > 0) {
    message("Verifying MD5 checksums for ", nrow(checkable), " downloaded file(s)...")
    checkable$local_md5 <- tools::md5sum(file.path(destfolder, checkable$filename))
    mismatches <- checkable[checkable$local_md5 != checkable$md5, ]
    if (nrow(mismatches) > 0) {
      warning(
        nrow(mismatches), " downloaded file(s) do not match tested checksums:\n",
        paste(sprintf("  %s\n    expected %s\n    got      %s",
                      mismatches$filename, mismatches$md5, mismatches$local_md5),
              collapse = "\n"), "\n",
        "  These may be updated versions. Run check_pgsourcefiles() for a full report.",
        call. = FALSE)
    } else {
      message("All ", nrow(checkable), " downloaded file(s) match tested checksums.")
    }
  }

  invisible(NULL)
}

#' Regenerate pgchecksum from locally verified files
#'
#' Developer-facing function that recomputes MD5 checksums for all raw source
#' files currently present in the raw data folder and saves them to
#' `data/pgchecksum.rda`. Replaces the manual `data_raw/pgchecksum.R` script.
#'
#' Only run this when you have a fully verified, clean set of downloaded files.
#' The resulting `pgchecksum` object is bundled with the package and used by
#' [check_pgsourcefiles()] and the optional checksum verification in [get_pgfile()].
#'
#' @param only_present Logical. If TRUE (default), only compute checksums for
#'   files currently present in the raw folder. If FALSE, stops if any metadata
#'   file is missing locally.
#'
#' @return A data.frame of checksums (invisibly). Also saves to `data/pgchecksum.rda`.
#' @export
#'
#' @examples
#' \dontrun{
#' pg_update_checksums()
#' }
pg_update_checksums <- function(only_present = TRUE) {
  rlang::check_installed("usethis", reason = "to save pgchecksum.rda")
  destfolder <- pg_rawfolder()
  file_info <- pg_rawfiles()
  file_info$exists <- file.exists(file.path(destfolder, file_info$filename))

  if (!only_present && !all(file_info$exists)) {
    missing <- file_info$filename[!file_info$exists]
    stop(
      sum(!file_info$exists), " file(s) not found locally (only_present=FALSE requires all files):\n",
      paste(missing, collapse = "\n"),
      call. = FALSE)
  }

  if (only_present) {
    file_info <- file_info[file_info$exists, ]
  }

  if (nrow(file_info) == 0) {
    stop("No local raw files found. Download files first with download_pg_rawdata().", call. = FALSE)
  }

  message("Computing MD5 for ", nrow(file_info), " file(s)...")
  pgchecksum <- file_info |>
    dplyr::mutate(md5 = tools::md5sum(file.path(destfolder, filename))) |>
    dplyr::select(source_name, source_version, id, filename, md5)

  usethis::use_data(pgchecksum, overwrite = TRUE)
  message("pgchecksum saved to data/pgchecksum.rda (", nrow(pgchecksum), " entries).")
  invisible(pgchecksum)
}
