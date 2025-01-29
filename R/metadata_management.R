parse_url <- function(url){
  if(is.na(url) || trimws(url) == "" || !is.character(url)){
    return("missing")
  }

  if(startsWith(url, "urls/")){
    fpath <- file.path("data", url)
    if(file.exists(fpath) && !dir.exists(fpath)){
      return("parsed url file")
    } else(
      return("missing url file")
    )
  }

  if(file.exists(url) && !dir.exists(url)){
    return("unparsed url file")
  }

  if(file.exists(url) && dir.exists(url)){
    return("directory")
  }

  if(RCurl::url.exists(url)){
    return("working url")
  }

  return("possibly non-working url")
}

parse_url_file <- function(fname, id){
  urls <- readLines(fname)
  fpath <- file.path("data", "urls", paste0(id, ".txt"))
  writeLines(urls, fpath)
  fpath <- file.path("urls", basename(fpath))
  return(fpath)
}

test_urls_in_file <- function(fname){
  urls <- readLines(fname)
  all(sapply(urls, RCurl::url.exists))
}

handle_url <- function(url, id, test = FALSE) {
  input_type <- parse_url(url)

  f <- switch (input_type,
    "missing" = function(url, id){
      warning("URL is missing.")
      return(list("entry" = NA, "exists" = NA))
    },
    "missing url file" = function(url, id){
      warning("URL file is missing.")
      return(list("entry" = NA, "exists" = NA))
    },
    "directory" = function(url, id){
      warning("URL is a local directory, not a URL.")
      return(list("entry" = NA, "exists" = NA))
    },
    "parsed url file" = function(url, id){
      url_file_path <- file.path("data", url)
      return(list("entry" = url, "exists" = test_urls_in_file(url_file_path)))
    },
    "unparsed url file" = function(url, id){
      url_file_path <- parse_url_file(url, id)
      return(list("entry" = url_file_path, "exists" = test_urls_in_file(url_file_path)))
    },
    "working url" = function(url, id){
      return(list("entry" = url, "exists" = TRUE))
    },
    "possibly non-working url" = function(url, id){
      warning("URL does not parse. Please review.")
      return(list("entry" = "NA", "exists" = NA))
    }
  )

  if(test){
    return(f)
  } else{
    return(f(url, id))
  }
}


#' Add source to PRIO-GRID meta-data
#'
#' @param source_name String. Full name of the source, including the institution hosting it.
#' @param source_version String. The version of the source, as noted by the creator. If none, use the publication year.
#' @param license String. The data license. If nothing is stated, then "All rights are reserved". Use the URL to licenses that are specific, and common strings for general licenses.
#' @param citation_keys String. The bibkey(s) of the citation. We use Zotero with Better Bibtex to organize our citations using this formula: auth.lower + shorttitle(3,3) + year. Separate with semi-colon if more than one.
#' @param website_url String. The URL to the most relevant online landing site for the data.
#' @param spatial_extent String. World, Multiple continents, Single continent, or Several countries (spread).
#' @param temporal_resolution String. Static, Higher than monthly, Monthly, Quarterly, Yearly, or Less than yearly.
#' @param aws_bucket String. The Amazon S3 bucket to data repository. Use if source uses Amazon S3.
#' @param aws_region String. The Amazon S3 region to data repository. Use if source uses Amazon S3.
#' @param download_url String. The URL to the data-file or path to local text-file with one URL per line.
#' @param prio_mirror String. The URL to the data-file or path to local text-file with one URL per line. Alternative download location hosted by PRIO.
#' @param tags String. Comma-separated tags used to sort and navigate the data sources.
#' @param reference_keys String. Bibkey(s) of other relevant references, e.g., articles discussing older versions, articles using the data, articles testing the data. Separate with semi-colon.
#'
#' @returns
#' @export
#'
#' @examples
add_source <- function(source_name,
                       source_version,
                       license,
                       website_url,
                       spatial_extent,
                       temporal_resolution,
                       citation_keys = NULL,
                       aws_bucket = NA,
                       aws_region = NA,
                       download_url = NA,
                       prio_mirror = NA,
                       tags = NULL,
                       reference_keys = NULL,
                       test = TRUE){

  is_character_but_not_empty <- function(x) is.character(x) && x != ""

  assertthat::assert_that(is_character_but_not_empty(source_name))
  assertthat::assert_that(is_character_but_not_empty(source_version))
  assertthat::assert_that(is_character_but_not_empty(license))
  assertthat::assert_that(is_character_but_not_empty(website_url))
  assertthat::assert_that(is_character_but_not_empty(spatial_extent))
  assertthat::assert_that(is_character_but_not_empty(temporal_resolution))
  assertthat::assert_that(is_character_but_not_empty(citation_keys) || is.null(citation_keys))
  assertthat::assert_that(is_character_but_not_empty(aws_bucket) || is.na(aws_bucket))
  assertthat::assert_that(is_character_but_not_empty(aws_region) || is.na(aws_region))
  assertthat::assert_that(is_character_but_not_empty(download_url) || is.na(download_url))
  assertthat::assert_that(is_character_but_not_empty(prio_mirror) || is.na(prio_mirror))
  assertthat::assert_that(is_character_but_not_empty(tags) || is.null(tags))
  assertthat::assert_that(is.logical(test))

  col_types <- readr::cols(
    id = readr::col_character(),
    source_name = readr::col_character(),
    source_version = readr::col_character(),
    license = readr::col_character(),
    citation_keys = readr::col_character(),
    aws_bucket = readr::col_character(),
    aws_region = readr::col_character(),
    download_url = readr::col_character(),
    website_url = readr::col_character(),
    tags = readr::col_character(),
    spatial_extent = readr::col_character(),
    temporal_resolution = readr::col_character(),
    reference_keys = readr::col_character(),
    prio_mirror = readr::col_character(),
    download_url_exists = readr::col_logical(),
    website_url_exists = readr::col_logical(),
    prio_mirror_exists = readr::col_logical(),
    created_at = readr::col_character()
  )
  csv_file <- "data_raw/sources.csv"
  sources <- readr::read_delim(csv_file, delim = "\t", col_types = col_types)

  download_url_obj <- handle_url(download_url)
  prio_mirror_obj <- handle_url(prio_mirror)

  website_url_type <- parse_url(website_url)
  if(website_url_type == "working url"){
    website_url_obj = list("entry" = website_url, "exists" = TRUE)
  } else{
    stop("Cannot validate website URL. Do you have internet connection?")
  }

  new_id <- uuid::UUIDgenerate()

  new_source = dplyr::tibble(
   "id" = new_id,
   "source_name" = source_name,
   "source_version" = source_version,
   "license" = license,
   "citation_keys" = citation_keys,
   "aws_bucket" = aws_bucket,
   "aws_region" = aws_region,
   "download_url" = download_url_obj$entry,
   "website_url" = website_url_obj$entry,
   "tags" = tags,
   "spatial_extent" = spatial_extent,
   "temporal_resolution" = temporal_resolution,
   "reference_keys" = reference_keys,
   "prio_mirror" = prio_mirror_obj$entry,
   "download_url_exists" = download_url_obj$exists,
   "website_url_exists" = website_url_obj$exists,
   "prio_mirror_exists" = prio_mirror_obj$exists,
   "created_at" = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  if(test){
    if(parse_url(download_url) == "unparsed url file"){
      unlink(download_url_obj$entry) # remove this file since it was just a test
    }
    if(parse_url(prio_mirror) == "unparsed url file"){
      unlink(prio_mirror_obj$entry) # remove this file since it was just a test
    }

    dplyr::bind_rows(sources, new_source)
    return(new_source)
  } else{
    updated_sources <- dplyr::bind_rows(sources, new_source)
    readr::write_delim(updated_data, csv_file, delim = "\t")
  }

}

#add_source("a", "b", "c", "d", "e", "f", test = T) |> View()
