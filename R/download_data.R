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

#' Default local filename for a download url
#'
#' The last path segment of the url, with any query string removed.
#'
#' @param url Character vector of urls.
#'
#' @return Character vector of filenames.
#' @keywords internal
pg_default_filename <- function(url){
  basename(url) |> stringr::str_remove("\\?.*")
}

#' Read a PRIO-GRID url list file
#'
#' Url list files in `inst/extdata/urls` hold one download url per line. A line
#' may optionally carry a tab-separated second column naming the file to store
#' the download under locally. That second column is only needed for sources
#' whose urls do not name the file they serve, such as
#' `https://ndownloader.figshare.com/files/17626052`. Use
#' [pg_resolve_filenames()] to generate it.
#'
#' @param path Character. Path to the url list file.
#'
#' @return data.frame with columns `url` and `filename`, where `filename` is NA
#'   for lines that do not state one.
#' @keywords internal
pg_read_url_list <- function(path){
  if(identical(path, "") || !file.exists(path)){
    stop("Url list file not found: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  parts <- stringr::str_split_fixed(lines, "\t", 2)
  filename <- trimws(parts[, 2])
  filename[!nzchar(filename)] <- NA_character_

  dplyr::tibble(url = trimws(parts[, 1]), filename = filename)
}

#' Format urls and filenames as url list file lines
#'
#' The filename column is only written where it differs from what
#' [pg_default_filename()] would derive, so that url lists for sources with
#' self-describing urls stay single-column.
#'
#' @param url Character vector of urls.
#' @param filename Character vector of filenames, or NULL.
#'
#' @return Character vector of lines.
#' @keywords internal
pg_format_url_list <- function(url, filename = NULL){
  if(is.null(filename)) return(url)

  explicit <- !is.na(filename) & filename != pg_default_filename(url)
  ifelse(explicit, paste(url, filename, sep = "\t"), url)
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
    if(startsWith(url, "urls/")){
      files <- pg_read_url_list(system.file("extdata", url, package = "priogrid"))
    } else{
      files <- dplyr::tibble(url = url, filename = NA_character_)
    }
    files$filename <- dplyr::coalesce(files$filename, pg_default_filename(files$url))
    return(files)
  }

  # The url-column can point to a file listing many urls, so expand it first.
  urls$files <- lapply(urls$url, parse_source_url)
  urls <- urls |>
    dplyr::select(-url) |>
    tidyr::unnest(cols = c(files)) |>
    dplyr::select(source_name, source_version, id, url, filename)

  collisions <- urls |>
    dplyr::count(id, filename) |>
    dplyr::filter(n > 1)

  if(nrow(collisions) > 0){
    stop("Files within a source must have unique names, but found duplicates:\n",
         paste0("  ", collisions$id, ": ", collisions$filename, collapse = "\n"), "\n",
         "  Give the colliding urls an explicit tab-separated filename in inst/extdata/urls/.",
         call. = FALSE)
  }

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
    n_partial <- sum(file.exists(paste0(full_file_path[!file_found], ".part")))
    if (pg_current_config()$automatic_download) {
      stop(sprintf(
        "%d file(s) for '%s' v%s not found after download attempt:\n  %s\n",
        sum(!file_found), source_name, source_version,
        paste(missing_names, collapse = "\n  ")),
        if (n_partial > 0) sprintf(
          "  %d of these were partly downloaded and can be resumed.\n", n_partial),
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
#'   `n_present`, `n_partial`, and `all_present`, or NULL if the raw data folder
#'   is not set. `n_partial` counts files whose download was interrupted and can
#'   be resumed by re-running [download_pg_rawdata()].
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
    fi$part_exists <- file.exists(paste0(file.path(destfolder, fi$filename), ".part"))
    fi |>
      dplyr::group_by(source_name, source_version) |>
      dplyr::summarise(
        n_files = dplyr::n(),
        n_present = sum(file_exists),
        n_partial = sum(part_exists),
        all_present = all(file_exists),
        .groups = "drop"
      )
  }, error = function(e) {
    message("Raw data folder not set. Use pg_set_rawfolder() to configure.")
    invisible(NULL)
  })
}

#' Curl handle options used for all PRIO-GRID downloads
#'
#' Identifies the client to the data providers we download from, and makes a
#' stalled transfer fail fast so that it can be resumed instead of hanging.
#'
#' @return Named list of options for [curl::new_handle()].
#' @keywords internal
pg_curl_opts <- function(){
  list(
    useragent = sprintf("priogrid/%s (R %s; https://github.com/prio-data/priogrid)",
                        packageVersion("priogrid"), getRversion()),
    connecttimeout = 30,
    low_speed_limit = 1024,
    low_speed_time = 60
  )
}

#' Look up a response header
#'
#' Returns the last occurrence, which after redirects is the one belonging to
#' the response that actually carried the data.
#'
#' @param headers Named list from [curl::parse_headers_list()].
#' @param name Character. Lower-case header name.
#'
#' @return Character string, or NA if the header is absent.
#' @keywords internal
pg_header_value <- function(headers, name){
  if(length(headers) == 0) return(NA_character_)
  hits <- headers[names(headers) == name]
  if(length(hits) == 0) return(NA_character_)
  trimws(as.character(hits[[length(hits)]]))
}

#' Parse the headers of one row of a curl download report
#'
#' @param report data.frame from [curl::multi_download()].
#' @param i Integer. Row number.
#'
#' @return Named list of headers, empty if unavailable.
#' @keywords internal
pg_response_headers <- function(report, i){
  headers <- report[["headers"]]
  if(is.null(headers) || is.na(i) || i > length(headers)) return(list())
  raw_headers <- headers[[i]]
  if(is.null(raw_headers) || length(raw_headers) == 0) return(list())
  tryCatch(curl::parse_headers_list(raw_headers), error = function(e) list())
}

#' Extract a filename from a Content-Disposition header
#'
#' Handles both the RFC 6266 extended form (`filename*=UTF-8''name`) and the
#' plain form (`filename="name"`), preferring the extended one.
#'
#' @param disposition Character. Content-Disposition header value.
#'
#' @return Character filename, or NA.
#' @keywords internal
pg_disposition_filename <- function(disposition){
  if(is.na(disposition)) return(NA_character_)

  extended <- stringr::str_match(disposition, "filename\\*\\s*=\\s*[^']*''([^;]+)")[, 2]
  quoted <- stringr::str_match(disposition, "filename\\s*=\\s*\"([^\"]+)\"")[, 2]
  bare <- stringr::str_match(disposition, "filename\\s*=\\s*([^;\"]+)")[, 2]

  name <- dplyr::coalesce(extended, quoted, bare)
  if(is.na(name)) return(NA_character_)

  name <- tryCatch(utils::URLdecode(trimws(name)), error = function(e) trimws(name))
  basename(stringr::str_remove(name, "\\?.*"))
}

#' Number of bytes the response says the finished file should have
#'
#' A resumed (206) response reports only the remaining bytes in Content-Length,
#' so the total is taken from Content-Range where possible and otherwise
#' reconstructed from the resume offset. Returns NA whenever the size cannot be
#' established (chunked responses, compressed transfers), in which case callers
#' must skip the check rather than treat the file as bad.
#'
#' @param headers Named list from [pg_response_headers()].
#' @param resumefrom Numeric. Byte offset the request resumed from.
#' @param status_code Integer. HTTP status of the response.
#'
#' @return Numeric number of bytes, or NA.
#' @keywords internal
pg_expected_bytes <- function(headers, resumefrom, status_code){
  encoding <- pg_header_value(headers, "content-encoding")
  if(!is.na(encoding) && !identical(tolower(encoding), "identity")) return(NA_real_)

  content_range <- pg_header_value(headers, "content-range")
  if(!is.na(content_range)){
    total <- stringr::str_match(content_range, "/\\s*(\\d+)\\s*$")[, 2]
    if(!is.na(total)) return(as.numeric(total))
  }

  content_length <- suppressWarnings(as.numeric(pg_header_value(headers, "content-length")))
  if(is.na(content_length)) return(NA_real_)

  # Only a 206 means the body is a continuation of what is already on disk.
  offset <- suppressWarnings(as.numeric(resumefrom))
  if(is.na(offset) || !identical(as.integer(status_code), 206L)) offset <- 0

  content_length + offset
}

#' Classify the outcome of a download attempt
#'
#' Separates failures worth retrying from ones that will never succeed, so that
#' a missing file does not burn every retry.
#'
#' @param report data.frame with `success` and `status_code` columns.
#'
#' @return Character vector of "ok", "permanent" or "transient".
#' @keywords internal
pg_classify_download <- function(report){
  success <- report$success
  status <- suppressWarnings(as.integer(report$status_code))
  permanent_status <- c(400L, 401L, 403L, 404L, 410L, 451L)

  dplyr::case_when(
    !is.na(success) & success ~ "ok",
    !is.na(status) & status %in% permanent_status ~ "permanent",
    TRUE ~ "transient"
  )
}

#' Seconds to wait before the next download attempt
#'
#' Exponential backoff with jitter, so that a server having a bad minute is not
#' hammered and concurrent clients do not retry in lockstep. A `Retry-After`
#' from the server raises the delay, but nothing exceeds `cap`.
#'
#' @param attempt Integer. Attempt number, starting at 1.
#' @param base Numeric. Base delay in seconds.
#' @param cap Numeric. Maximum delay in seconds.
#' @param retry_after Numeric. Seconds requested by the server, or NA.
#'
#' @return Numeric delay in seconds.
#' @keywords internal
pg_retry_delay <- function(attempt, base = 2, cap = 120, retry_after = NA_real_){
  delay <- base * 2^(attempt - 1)
  delay <- delay + stats::runif(length(delay), 0, base)

  retry_after <- suppressWarnings(as.numeric(retry_after))
  delay <- ifelse(is.na(retry_after), delay, pmax(delay, retry_after))

  pmin(delay, cap)
}

#' Download one batch of files
#'
#' Writes to `<destfile>.part` so that an interrupted transfer never leaves a
#' truncated file under the name callers treat as complete, and so that a rerun
#' can resume it.
#'
#' Note that [curl::multi_download()] reports the *effective* url, i.e. the one
#' reached after redirects, so the request url cannot be used to match results
#' back to requests. `destfile` comes back exactly as passed and is used instead.
#'
#' [curl::multi_download()] transfers everything it is given in parallel and
#' exposes no connection limit, so the number of urls passed in is what bounds
#' concurrency. Callers keep that number small rather than this function.
#'
#' @param urls Character vector of urls to download.
#' @param destfiles Character vector of final file paths.
#' @param progress Logical. Whether to show a progress bar.
#'
#' @return data.frame with one row per requested file.
#' @keywords internal
pg_download_batch <- function(urls, destfiles, progress = TRUE){
  partfiles <- paste0(destfiles, ".part")

  report <- do.call(curl::multi_download, c(
    list(urls = urls, destfiles = partfiles, resume = TRUE, progress = progress),
    pg_curl_opts()
  ))

  # Match on the destination path, but do not depend on how curl chose to spell
  # it: normalizePath() resolves symlinks only for files that already exist, so
  # the same path can come back in two forms depending on whether the download
  # resumed. Row order is the last resort, and only when the counts agree.
  idx <- match(partfiles, report$destfile)
  unresolved <- is.na(idx)
  if(any(unresolved)){
    idx[unresolved] <- match(normalizePath(partfiles[unresolved], mustWork = FALSE),
                             report$destfile)
  }
  if(anyNA(idx)){
    if(nrow(report) != length(partfiles)){
      stop("curl returned ", nrow(report), " result(s) for ", length(partfiles),
           " requested file(s), so downloads cannot be matched to requests.",
           call. = FALSE)
    }
    idx <- seq_along(partfiles)
  }

  headers <- lapply(idx, function(i) pg_response_headers(report, i))
  resumefrom <- if(is.null(report[["resumefrom"]])) rep(0, length(idx)) else report$resumefrom[idx]

  out <- dplyr::tibble(
    url = urls,
    destfile = destfiles,
    partfile = partfiles,
    success = report$success[idx],
    status_code = suppressWarnings(as.integer(report$status_code[idx])),
    effective_url = report$url[idx],
    error = as.character(report$error[idx])
  )

  out$expected_bytes <- vapply(seq_along(idx), function(k){
    pg_expected_bytes(headers[[k]], resumefrom[k], out$status_code[k])
  }, numeric(1))

  out$retry_after <- vapply(headers, function(h){
    suppressWarnings(as.numeric(pg_header_value(h, "retry-after")))
  }, numeric(1))

  # What the server says the file is called. Content-Disposition is the server
  # naming the file outright; a name read off the effective url is a guess,
  # because storage urls routinely end in an opaque object key.
  from_header <- vapply(headers, function(h){
    pg_disposition_filename(pg_header_value(h, "content-disposition"))
  }, character(1))

  out$name_from_header <- !is.na(from_header)
  out$resolved_name <- ifelse(out$name_from_header, from_header,
                              ifelse(is.na(out$effective_url), NA_character_,
                                     pg_default_filename(out$effective_url)))

  out
}

#' Download the raw-data for PRIO-GRID
#'
#' Before running this, you need to set the folder using pg_set_rawfolder("path/to/folder")
#'
#' @details
#' Files are written to `<filename>.part` and only renamed into place once the
#' transfer has finished, so a file under its final name always means a complete
#' download. An interrupted run leaves `.part` files behind, and calling the
#' function again resumes them.
#'
#' Two independent checks run on each downloaded file. The transfer is checked
#' against the size the server announced, and a short read is retried. The file
#' is separately compared against the MD5 in `pgchecksum` where one exists; that
#' comparison is advisory and never blocks a download, because sources have no
#' reference checksum until PRIO-GRID has been built from them once. See
#' [pg_update_checksums()] and [check_pgsourcefiles()].
#'
#' Files downloaded by earlier versions of this function were written directly
#' to their final name, so a file truncated before this change still looks
#' complete. Use [check_pgsourcefiles()] to find those.
#'
#' @param file_info A data.frame with the same structure as the result from [pg_rawfiles()]. If file_info is null (default),
#'   then file_info will be all data returned from [pg_rawfiles()].
#' @param overwrite Whether or not to download and overwrite files already in local folder.
#' @param batch_size Integer. Number of files per download batch. Default 20.
#' @param max_retry Integer. Maximum number of retry attempts for failed downloads. Default 10.
#' @param max_concurrent Integer. Upper bound on simultaneous transfers. Default 4.
#'   A batch is transferred in parallel, so the effective batch is
#'   `min(batch_size, max_concurrent)` and this is the argument that decides how
#'   hard a server is hit.
#' @param retry_base_delay Numeric. Base seconds for exponential backoff between retries. Default 2.
#' @param retry_max_delay Numeric. Maximum seconds to wait between retries. Default 120.
#' @param verify Logical. Whether to check downloaded files against the size the
#'   server announced and against [pgchecksum]. Default TRUE.
#' @param quiet Logical. Suppress progress messages. Defaults to the inverse of
#'   `verbose` in [pg_current_config()].
#'
#' @return data.frame (invisibly). One row per file in `file_info`, with columns
#'   `source_name`, `source_version`, `id`, `filename`, `url`, `status`
#'   ("ok", "failed" or "skipped"), `attempts`, `status_code`, `bytes`,
#'   `md5_ok`, `resolved_name`, `name_drift` and `error`.
#' @export
#'
#' @examples
#' files_to_download <- pg_rawfiles() |> dplyr::filter(id == "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
#' # download_pg_rawdata(overwrite = TRUE, file_info = files_to_download)
download_pg_rawdata <- function(file_info = NULL,
                                overwrite = FALSE,
                                batch_size = 20,
                                max_retry = 10,
                                max_concurrent = 4,
                                retry_base_delay = 2,
                                retry_max_delay = 120,
                                verify = TRUE,
                                quiet = !pg_current_config()$verbose){
  destfolder <- pg_rawfolder()

  if(!dir.exists(destfolder)){
    if(interactive()){
      accept <- readline(paste("Destination folder", destfolder, "does not exist. Do you want to create? (Y)es: "))
      if(!tolower(accept) %in% c("y", "yes")){
        return(message("No folder to store data."))
      }
    }
    dir.create(destfolder, recursive = TRUE)
  }

  if(is.null(file_info)){
      file_info <- pg_rawfiles()
  }

  required_columns <- c("source_name", "source_version", "id", "url", "filename")
  missing_columns <- setdiff(required_columns, names(file_info))
  if(length(missing_columns) > 0){
    stop("file_info is missing column(s): ", paste(missing_columns, collapse = ", "), "\n",
         "  It must have the same structure as the result from pg_rawfiles().",
         call. = FALSE)
  }

  file_info <- dplyr::as_tibble(file_info)
  file_info$destfile <- file.path(destfolder, file_info$filename)
  file_info$partfile <- paste0(file_info$destfile, ".part")

  # A .part file means an earlier run was interrupted. Pick those up unasked.
  needs_download <- overwrite | !file.exists(file_info$destfile) | file.exists(file_info$partfile)

  todo <- file_info[needs_download, ]
  skipped <- pg_download_result(file_info[!needs_download, ], status = "skipped")

  if(nrow(todo) == 0){
    if(!quiet) message("All raw-files are already in ", destfolder)
    return(invisible(skipped))
  }

  subdirs <- unique(dirname(todo$destfile))
  for(newdir in subdirs[!dir.exists(subdirs)]){
    dir.create(newdir, recursive = TRUE)
  }

  result <- pg_download_result(todo, status = "failed")
  pending <- seq_len(nrow(todo))

  # curl runs a whole call in parallel with no connection limit of its own, so
  # the batch is what keeps the number of simultaneous transfers down.
  chunk_size <- max(1L, min(batch_size, max_concurrent))

  for(attempt in seq_len(max_retry + 1L)){
    batches <- split(pending, ceiling(seq_along(pending) / chunk_size))

    reports <- list()
    for(b in seq_along(batches)){
      rows <- batches[[b]]
      if(!quiet) message("Processing batch: ", b, "/", length(batches))
      reports[[b]] <- pg_download_batch(todo$url[rows], todo$destfile[rows],
                                        progress = !quiet)
      reports[[b]]$row <- rows
    }
    report <- dplyr::bind_rows(reports)
    report$outcome <- pg_classify_download(report)
    report$bytes <- ifelse(file.exists(report$partfile), file.size(report$partfile), NA_real_)

    # A 416 means the server has nothing to add to what is already on disk, so a
    # resumed transfer of exactly the right length is in fact finished.
    size_known <- !is.na(report$expected_bytes) & !is.na(report$bytes)
    finished_resume <- size_known & !is.na(report$status_code) &
      report$status_code == 416L & report$bytes == report$expected_bytes
    report$outcome[finished_resume] <- "ok"
    report$error[finished_resume] <- NA_character_

    # The server told us how big the file is and we got a different number.
    wrong_size <- report$outcome == "ok" & size_known &
      report$bytes != report$expected_bytes
    report$outcome[wrong_size] <- "transient"
    report$error[wrong_size] <- sprintf("incomplete transfer: %.0f of %.0f bytes",
                                        report$bytes[wrong_size],
                                        report$expected_bytes[wrong_size])

    # A file longer than announced cannot be resumed from, so start it over.
    unlink(report$partfile[which(wrong_size & report$bytes > report$expected_bytes)])

    complete <- which(report$outcome == "ok")
    for(k in complete){
      if(file.exists(report$destfile[k])) unlink(report$destfile[k])
      if(!file.rename(report$partfile[k], report$destfile[k])){
        report$outcome[k] <- "transient"
        report$error[k] <- "could not move completed download into place"
      }
    }

    # An error response body is not a partial download, so do not resume from it.
    discard <- report$outcome != "ok" & !is.na(report$status_code) & report$status_code >= 400
    unlink(report$partfile[which(discard)])

    result$attempts[report$row] <- attempt
    result$status_code[report$row] <- report$status_code
    result$bytes[report$row] <- report$bytes
    result$error[report$row] <- report$error
    result$resolved_name[report$row] <- report$resolved_name
    result$name_drift[report$row] <- report$name_from_header &
      !is.na(report$resolved_name) &
      report$resolved_name != basename(report$destfile)
    result$status[report$row] <- ifelse(report$outcome == "ok", "ok", "failed")

    pending <- report$row[report$outcome == "transient"]
    if(length(pending) == 0 || attempt > max_retry) break

    delay <- pg_retry_delay(attempt, retry_base_delay, retry_max_delay,
                            pg_max_or_na(report$retry_after[report$outcome == "transient"]))
    if(!quiet){
      message(length(pending), " file(s) did not finish. Waiting ", round(delay), "s before retry ",
              attempt, "/", max_retry, ".")
    }
    Sys.sleep(delay)
  }

  if(verify) result <- pg_verify_checksums(result, destfolder, quiet = quiet)

  drifted <- result[which(result$name_drift & result$status == "ok"), ]
  if(nrow(drifted) > 0){
    warning(
      nrow(drifted), " file(s) are served under a different name than PRIO-GRID expects:\n",
      paste0("  ", basename(drifted$filename), " <- ", drifted$resolved_name, collapse = "\n"), "\n",
      "  The source may have been reorganised upstream. If the served name is correct,\n",
      "  record it in inst/extdata/urls/ with pg_resolve_filenames().",
      call. = FALSE)
  }

  failed <- result[result$status == "failed", ]
  if(nrow(failed) > 0){
    warning(
      nrow(failed), " file(s) could not be downloaded after ", max_retry, " retries:\n",
      paste0("  ", failed$filename, " (",
             ifelse(is.na(failed$error), paste("HTTP", failed$status_code), failed$error), ")",
             collapse = "\n"), "\n",
      "  Any partial downloads were kept as .part files. Run download_pg_rawdata() again to resume.",
      call. = FALSE)
  } else if(!quiet){
    message("Downloaded ", sum(result$status == "ok"), " file(s) to ", destfolder)
  }

  invisible(dplyr::bind_rows(result, skipped))
}

#' Build an empty per-file download summary
#'
#' @param file_info data.frame of files, as from [pg_rawfiles()].
#' @param status Character. Initial status for every row.
#'
#' @return data.frame with the columns documented in [download_pg_rawdata()].
#' @keywords internal
pg_download_result <- function(file_info, status){
  dplyr::tibble(
    source_name = file_info$source_name,
    source_version = file_info$source_version,
    id = file_info$id,
    filename = file_info$filename,
    url = file_info$url,
    status = rep(status, nrow(file_info)),
    attempts = 0L,
    status_code = NA_integer_,
    bytes = NA_real_,
    md5_ok = NA,
    resolved_name = NA_character_,
    name_drift = NA,
    error = NA_character_
  )
}

#' Largest non-missing value, or NA if there are none
#'
#' [base::max()] warns and returns -Inf for an all-NA input.
#'
#' @param x Numeric vector.
#'
#' @return Numeric scalar.
#' @keywords internal
pg_max_or_na <- function(x){
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA_real_)
  max(x)
}

#' Compare downloaded files against the tested checksums
#'
#' Advisory only. Files without a reference in [pgchecksum] keep `md5_ok` as NA
#' and are not treated as problems, which is what makes it possible to download
#' a source for the first time and generate its checksums afterwards.
#'
#' @param result data.frame from [pg_download_result()].
#' @param destfolder Character. Raw data folder.
#' @param quiet Logical. Suppress progress messages.
#'
#' @return `result` with `md5_ok` filled in where a reference exists.
#' @keywords internal
pg_verify_checksums <- function(result, destfolder, quiet = FALSE){
  downloaded <- result[result$status == "ok", ]
  if(nrow(downloaded) == 0) return(result)

  checkable <- dplyr::inner_join(
    downloaded[, c("source_name", "source_version", "id", "filename")],
    pgchecksum, by = c("source_name", "source_version", "id", "filename"))
  if(nrow(checkable) == 0) return(result)

  if(!quiet) message("Verifying MD5 checksums for ", nrow(checkable), " downloaded file(s)...")
  checkable$local_md5 <- unname(tools::md5sum(file.path(destfolder, checkable$filename)))
  checkable$md5_ok <- checkable$local_md5 == checkable$md5

  result$md5_ok[match(checkable$filename, result$filename)] <- checkable$md5_ok

  mismatches <- checkable[!is.na(checkable$md5_ok) & !checkable$md5_ok, ]
  if(nrow(mismatches) > 0){
    warning(
      nrow(mismatches), " downloaded file(s) do not match tested checksums:\n",
      paste(sprintf("  %s\n    expected %s\n    got      %s",
                    mismatches$filename, mismatches$md5, mismatches$local_md5),
            collapse = "\n"), "\n",
      "  These may be updated versions. Run check_pgsourcefiles() for a full report.",
      call. = FALSE)
  } else if(!quiet){
    message("All ", nrow(checkable), " downloaded file(s) match tested checksums.")
  }

  result
}

#' Resolve the real filenames behind a source's download urls
#'
#' Some sources serve files from urls that do not name them, such as
#' `https://ndownloader.figshare.com/files/17626052`. Left alone, those land on
#' disk as `17626052`. This developer-facing helper asks each server what the
#' file is actually called and records the answer as a tab-separated second
#' column in the source's url list, so that [pg_rawfiles()] keeps deriving local
#' paths offline and the bundled [pgchecksum] stays valid.
#'
#' The name is taken from the `Content-Disposition` header when the server sends
#' one, then from the url the request ended up at after redirects, then from the
#' stated url. None of these is reliable for every provider: sources behind a
#' login redirect to the login page, and GitHub redirects to storage urls with
#' opaque object keys. Read the proposed table before writing, which is why this
#' is run by a maintainer and committed rather than run by users.
#'
#' @param id Character. Source id (UUID), as in [pgsources].
#' @param write Logical. If TRUE, rewrite the url list file under `inst/extdata`.
#'   Defaults to FALSE so that the proposal can be inspected first.
#' @param delay Numeric. Seconds to wait between requests. Default 1.
#'
#' @return data.frame of urls, current filenames and resolved filenames (invisibly).
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' priogrid:::pg_resolve_filenames("d99fbea7-2a01-4221-b900-29a58d33f591")
#' priogrid:::pg_resolve_filenames("d99fbea7-2a01-4221-b900-29a58d33f591", write = TRUE)
#' }
pg_resolve_filenames <- function(id, write = FALSE, delay = 1){
  source_row <- pgsources[pgsources$id == id, ]
  if(nrow(source_row) != 1){
    stop("No single source in pgsources with id '", id, "'.", call. = FALSE)
  }

  url_ref <- if(!is.na(source_row$prio_mirror)) source_row$prio_mirror else source_row$download_url
  if(is.na(url_ref) || !startsWith(url_ref, "urls/")){
    stop("Source '", id, "' does not use a url list file, so there is nothing to resolve.",
         call. = FALSE)
  }

  url_list <- pg_read_url_list(system.file("extdata", url_ref, package = "priogrid"))

  resolved <- character(length(url_list$url))
  for(i in seq_along(url_list$url)){
    message("Resolving ", i, "/", length(url_list$url), ": ", url_list$url[i])
    resolved[i] <- pg_resolve_one_filename(url_list$url[i])
    if(i < length(url_list$url)) Sys.sleep(delay)
  }

  proposal <- dplyr::tibble(
    url = url_list$url,
    current = dplyr::coalesce(url_list$filename, pg_default_filename(url_list$url)),
    resolved = resolved
  )
  proposal$changed <- !is.na(proposal$resolved) & proposal$resolved != proposal$current
  proposal$suspicious <- pg_suspicious_filename(proposal$resolved)

  print(as.data.frame(proposal[, c("current", "resolved", "changed", "suspicious")]))

  if(any(proposal$suspicious)){
    warning(sum(proposal$suspicious), " resolved name(s) look wrong (no extension, or a login\n",
            "  endpoint rather than a file). Check them before writing.", call. = FALSE)
  }

  if(write){
    fpath <- file.path("inst/extdata", url_ref)
    if(!dir.exists(dirname(fpath))){
      stop("Cannot find ", dirname(fpath), ". Run this from the package source directory.",
           call. = FALSE)
    }
    filename <- ifelse(is.na(proposal$resolved), url_list$filename, proposal$resolved)
    writeLines(pg_format_url_list(proposal$url, filename), fpath)
    message("Wrote ", fpath, ". Review the diff, then rebuild and run pg_update_checksums().")
  }

  invisible(proposal)
}

#' Ask a server what a single file is called
#'
#' Requests only the first byte, so this is cheap even for very large files.
#'
#' @param url Character. A single url.
#'
#' @return Character filename, or NA if the request failed.
#' @keywords internal
pg_resolve_one_filename <- function(url){
  handle <- curl::new_handle()
  do.call(curl::handle_setopt,
          c(list(handle = handle), pg_curl_opts(),
            list(range = "0-0", followlocation = TRUE)))

  response <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) NULL)
  if(is.null(response)) return(NA_character_)

  headers <- tryCatch(curl::parse_headers_list(response$headers), error = function(e) list())

  from_disposition <- pg_disposition_filename(pg_header_value(headers, "content-disposition"))
  if(!is.na(from_disposition)) return(from_disposition)

  pg_default_filename(response$url)
}

#' Flag filenames that do not look like data files
#'
#' Catches the two ways name resolution goes wrong in practice: a redirect to a
#' login endpoint, and a storage url whose path is an opaque object key.
#'
#' @param filename Character vector.
#'
#' @return Logical vector.
#' @keywords internal
pg_suspicious_filename <- function(filename){
  is.na(filename) |
    !nzchar(tools::file_ext(filename)) |
    grepl("login|oauth|authorize|signin|callback", filename, ignore.case = TRUE)
}

#' Rename raw files left under a previous naming convention
#'
#' When a source's url list gains explicit filenames, files already downloaded
#' under the name derived from the url would otherwise be re-downloaded and the
#' old copies orphaned. This renames them in place instead.
#'
#' @param file_info A data.frame as from [pg_rawfiles()]. Defaults to all of it.
#' @param dry_run Logical. If TRUE (default), only report what would be renamed.
#'
#' @return data.frame of renames (invisibly).
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' priogrid:::pg_migrate_rawfiles()
#' priogrid:::pg_migrate_rawfiles(dry_run = FALSE)
#' }
pg_migrate_rawfiles <- function(file_info = NULL, dry_run = TRUE){
  destfolder <- pg_rawfolder()
  if(is.null(file_info)) file_info <- pg_rawfiles()

  old <- file.path(destfolder, dirname(file_info$filename), pg_default_filename(file_info$url))
  new <- file.path(destfolder, file_info$filename)

  # Interrupted downloads are carried over as well, so that a part-finished
  # large file does not have to start again from zero under its new name.
  candidates <- dplyr::tibble(
    from = c(old, paste0(old, ".part")),
    to = c(new, paste0(new, ".part"))
  )

  renameable <- candidates$from != candidates$to &
    file.exists(candidates$from) & !file.exists(candidates$to)
  renames <- candidates[renameable, ]

  if(nrow(renames) == 0){
    message("Nothing to migrate.")
    return(invisible(renames))
  }

  if(dry_run){
    message(nrow(renames), " file(s) would be renamed. Re-run with dry_run = FALSE to apply:")
    message(paste0("  ", basename(renames$from), " -> ", basename(renames$to), collapse = "\n"))
  } else {
    renames$ok <- file.rename(renames$from, renames$to)
    message("Renamed ", sum(renames$ok), " of ", nrow(renames), " file(s).")
  }

  invisible(renames)
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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' priogrid:::pg_update_checksums()
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
