#library(R6)
#library(checkmate)
#library(RCurl)

#' Source Class
#'
#' @description
#' An R6 Class representing a data source with comprehensive validation. Only for use in dev-mode (using devtools::load_all).
#'
Source <- R6::R6Class("Source",
  public = list(
    #' @description
    #' Create a new Source object
    #'
    #' @param source_name String. Full name of the source
    #' @param source_version String. Version of the source
    #' @param license String. Data license
    #' @param website_url String. URL to landing page
    #' @param spatial_extent String. One of predefined spatial extents
    #' @param temporal_resolution String. One of predefined temporal resolutions
    #' @param citation_keys String. Optional. Bibkey(s) of citations
    #' @param aws_bucket String. Optional. Amazon S3 bucket
    #' @param aws_region String. Optional. Amazon S3 region
    #' @param download_url String. Optional. URL to data file
    #' @param prio_mirror String. Optional. Alternative download location
    #' @param tags String. Optional. Comma-separated tags
    #' @param reference_keys String. Optional. Other relevant bibkeys
    #'
    #' @examples
    #' new_source <- Source$new(
    #'   source_name = "my new source",
    #'   source_version = "v1.0",
    #'   license = "CC BY 4.0",
    #'   website_url = "www.example.com",
    #'   spatial_extent = "World",
    #'   temporal_resolution = "Yearly",
    #'   citation_keys = "doeNewSource2025",
    #'   download_url = "www.example.com/path/to/my/new/source/file.csv",
    #'   tags = "test"
    #' )
    #' new_source # gives warning that doeNewSource2025 does not exist in the bibliography
    initialize = function(source_name,
                          source_version,
                          license,
                          website_url,
                          spatial_extent,
                          temporal_resolution,
                          citation_keys = NA_character_,
                          aws_bucket = NA_character_,
                          aws_region = NA_character_,
                          download_url = NA_character_,
                          prio_mirror = NA_character_,
                          tags = NA_character_,
                          reference_keys = NA_character_,
                          bib_path = "inst/REFERENCES.bib") {

      if(base::missing(source_name)) stop("`source_name´ is required")
      if(base::missing(source_version)) stop("`source_version´ is required")
      if(base::missing(license)) stop("`license´ is required")
      if(base::missing(website_url)) stop("`website_url´ is required")
      if(base::missing(spatial_extent)) stop("`spatial_extent´ is required")
      if(base::missing(temporal_resolution)) stop("`temporal_resolution´ is required")

      valid_spatial_extents = c("World", "Multiple continents", "Single continent", "Several countries")
      if(!checkmate::test_choice(spatial_extent, valid_spatial_extents)){
        valid_spatial_extents <- paste("- ", valid_spatial_extents, collapse = "\n")
        stop(sprintf("Invalid spatial extent `%s´: Must be one of:\n%s.", spatial_extent, valid_spatial_extents))
      }

      valid_temporal_resolutions = c("Static", "Higher than monthly", "Monthly", "Quarterly", "Yearly", "Less than yearly")
      if(!checkmate::test_choice(temporal_resolution, valid_temporal_resolutions)){
        valid_temporal_resolutions <- paste("- ", valid_temporal_resolutions, collapse = "\n")
        stop(sprintf("Invalid temporal resolution `%s´: Must be one of:\n%s.", temporal_resolution, valid_temporal_resolutions))
      }

      if(grepl(";", tags)){
        stop("`tags´ should be comma-separated, not semi-colon.")
      }

      if(grepl(",", citation_keys)){
        stop("`citation_keys´ should be semi-colon separated, not comma-separated.")
      }

      if(grepl(",", reference_keys)){
        stop("`reference_keys´ should be semi-colon separated, not comma-separated.")
      }

      private$data$id <- uuid::UUIDgenerate()
      private$data$source_name <- source_name
      private$data$source_version <- source_version
      private$data$license <- license
      private$data$website_url <- website_url
      private$data$spatial_extent <- spatial_extent
      private$data$temporal_resolution <- temporal_resolution

      private$citations <- private$handle_references(citation_keys, bib_path)
      private$data$citation_keys <- private$citations$updated_reference_string
      private$data$aws_bucket <- aws_bucket
      private$data$aws_region <- aws_region
      private$data$tags <- tags

      private$other_references <- private$handle_references(reference_keys, bib_path)
      private$data$reference_keys <- private$other_references$updated_reference_string

      # Handle URLs
      download_result <- private$handle_download_url(download_url, type = "urls")
      private$url_data$download <- download_result$urls
      private$data$download_url <- download_result$url
      private$data$download_url_exists <- download_result$valid

      prio_result <- private$handle_download_url(prio_mirror, type = "prio_mirror_urls")
      private$url_data$prio <- prio_result$urls
      private$data$prio_mirror <- prio_result$url
      private$data$prio_mirror_exists <- prio_result$valid
      private$data$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      private$validate_website_url()
    },

    get_existing_tags = function(){
      lapply(pgsources$tags |> unique(), strsplit, split = ",\\s*") |> unlist() |> unique() |> sort()
    },

    get_existing_licenses = function(){
      pgsources$license |> unique() |> sort()
    },

    get_url_path = function() {
      return(private$save_url_file())
    },

    set_download_url = function(url) {
      private$data$download_url <- url
      invisible(self)
    },

    to_tibble = function() {
      dplyr::tibble(
        id = private$data$id,
        source_name = private$data$source_name,
        source_version = private$data$source_version,
        license = private$data$license,
        citation_keys = private$data$citation_keys,
        aws_bucket = private$data$aws_bucket,
        aws_region = private$data$aws_region,
        download_url = private$data$download_url,
        website_url = private$data$website_url,
        tags = private$data$tags,
        spatial_extent = private$data$spatial_extent,
        temporal_resolution = private$data$temporal_resolution,
        reference_keys = private$data$reference_keys,
        prio_mirror = private$data$prio_mirror,
        download_url_exists = private$data$download_url_exists,
        website_url_exists = private$data$website_url_exists,
        prio_mirror_exists = private$data$prio_mirror_exists,
        created_at = private$data$created_at
      )
    },

    print = function(bib_path = "inst/REFERENCES.bib") {
      # ANSI color codes for terminal output
      green <- "\033[32m"
      red <- "\033[31m"
      reset <- "\033[0m"

      cat("Source:", private$data$source_name, "\n")
      cat("Version:", private$data$source_version, "\n")
      cat("License:", private$data$license, "\n")
      if(!private$data$license %in% self$get_existing_licenses()){
        existing_licenses <- paste("- ", self$get_existing_licenses(), collapse = "\n")
        cat(sprintf("%s%s%s is not among the already registrered licenses. Are you sure the license is not any of the ones in this list?\n%s\n",
                    red, private$data$license, reset, existing_licenses))
      }

      private$display_citation_results(private$citations, "Citations:")
      private$display_citation_results(private$other_references, "Other references:")

      cat("URL Status:\n")
      if (!private$data$website_url_exists) {
        cat(sprintf("- Website URL: %s%s%s not accessible. \n", red, private$data$website_url, reset))
      } else{
        cat(sprintf("- Website URL: %s%s%s accessible. \n", green, private$data$website_url, reset))
      }
      if (!private$data$download_url_exists) {
        cat(sprintf("- Download URL/URLs: %s%s%s not accessible. \n", red, private$data$download_url, reset))
      } else{
        cat(sprintf("- Download URL/URLs: %s%s%s accessible. \n", green, private$data$download_url, reset))
      }
      if (!private$data$prio_mirror_exists) {
        cat(sprintf("- PRIO mirror URL/URLs: %s%s%s not accessible. \n", red, private$data$prio_mirror, reset))
      } else{
        cat(sprintf("- PRIO mirror URL/URLs: %s%s%s accessible. \n", green, private$data$prio_mirror, reset))
      }

      tags <- strsplit(private$data$tags, split = ",\\s*") |> unlist() |> unique() |> sort()
      new_tags <- tags[!(tags %in% self$get_existing_tags())]
      if(length(new_tags) > 0){
        cat("Tags:\n")
        existing_tags <- paste("- ", self$get_existing_tags(), collapse = "\n")
        new_tags <- paste(new_tags, collapse = ", ")
        cat(sprintf("These tags are not in the system: %s%s%s. Could you use any of the existing tags instead (not required)?\n%s\n",
                    red, new_tags, reset, existing_tags))
      }

      cat("\nCreated at:", private$data$created_at, "\n")
      invisible(self)
    }
  ),

  private = list(
    input_data = NULL,
    data = list(),
    url_data = list(download = NULL, prio = NULL),
    citations = list(),
    other_references = list(),

    handle_references = function(reference_string, bib_path){
      if (is.null(reference_string) || is.na(reference_string) || trimws(reference_string) == "") {
        return(list(reference_string = NA_character_,
                    updated_reference_string = NA_character_,
                    found_citations = character(),
                    missing_citations = character()))
      }

      reference_keys <- unlist(strsplit(reference_string, ";\\s*"))
      reference_keys <- lapply(reference_keys, trimws)

      bib <- RefManageR::ReadBib(bib_path)
      citations <- sapply(reference_keys, function(key) RefManageR::Cite(bib, key))
      names(citations) <- reference_keys

      missing_citations <- citations[citations == ""]
      found_citations <- citations[citations != ""]

      updated_reference_string <- names(found_citations) |> paste(collapse = ";")

      return(list(reference_string = reference_string,
                  updated_reference_string = updated_reference_string,
                  found_citations = found_citations,
                  missing_citations = missing_citations))
    },

    display_citation_results = function(reference_results, type) {
      # Check if input is valid
      if (!is.list(reference_results) ||
          !all(c("found_citations", "missing_citations") %in% names(reference_results))) {
        stop("Input must be the output of handle_references function")
      }

      # ANSI color codes for terminal output
      green <- "\033[32m"
      red <- "\033[31m"
      reset <- "\033[0m"

      cat(paste(type, "\n"))
      # Display found citations
      if (length(reference_results$found_citations) > 0) {
        for (i in seq_along(reference_results$found_citations)) {
          key <- names(reference_results$found_citations)[i]
          citation <- reference_results$found_citations[i]
          cat(sprintf("%s- %s%s: %s\n", green, key, reset, citation))
        }
      } else {
        cat("- No bibliography-keys supplied.\n")
      }

      # Display missing citations
      if (length(reference_results$missing_citations) > 0) {
        for (i in seq_along(reference_results$missing_citations)) {
          key <- names(reference_results$missing_citations)[i]
          cat(sprintf("%s- %s%s: Citation not found in bibliography\n", red, key, reset))
        }
      }
    },

    handle_download_url = function(url, type) {
      if(!curl::has_internet()){
        stop("Internet not available. Please rerun with internet connection.")
      }
      # Handle NA, NULL, empty cases
      if (is.null(url) || is.na(url) || trimws(url) == "") {
        return(list(url = NA_character_, urls = NULL, valid = FALSE))
      }

      # Handle directory case
      if (dir.exists(url)) {
        return(list(
          url = NA_character_,
          urls = NULL,
          valid = FALSE
        ))
      }

      # Handle urls/ file case
      if (startsWith(url, "urls/")) {
        file_path <- file.path("data", url)
        if (!file.exists(file_path)) {
          return(list(
            url = NA_character_,
            urls = NULL,
            valid = FALSE
          ))
        }
        urls <- readLines(file_path)
        urls_valid <- all(sapply(urls, function(u) {
          tryCatch(RCurl::url.exists(u), error = function(e) FALSE)
        }))
        return(list(url = url, urls = urls, valid = urls_valid))
      }

      # Handle local file case
      if (file.exists(url)) {
        urls <- readLines(url)
        urls_valid <- all(sapply(urls, function(u) {
          tryCatch(RCurl::url.exists(u), error = function(e) FALSE)
        }))
        return(list(
          url = file.path(type, paste0(private$data$id, ".txt")),
          urls = urls,
          valid = urls_valid
        ))
      }

      # Handle direct URL case
      url_valid <- tryCatch(RCurl::url.exists(url), error = function(e) FALSE)

      return(list(
        url = url,
        urls = c(url),
        valid = url_valid
      ))
    },

    save_url_file = function() {
      urls_saved <- FALSE

      if (!is.null(private$url_data$download)) {
        fpath <- file.path("inst/extdata", private$data$download_url)
        dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)
        writeLines(private$url_data$download, fpath)
        urls_saved <- TRUE
      }

      if (!is.null(private$url_data$prio)) {
        fpath <- file.path("inst/extdata", private$data$prio_mirror)
        dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)
        writeLines(private$url_data$prio, fpath)
        urls_saved <- TRUE
      }

      return(urls_saved)
    },

    validate_website_url = function() {
      private$data$website_url_exists <- tryCatch(
        RCurl::url.exists(private$data$website_url),
        error = function(e) FALSE
      )
    }
  )
)
