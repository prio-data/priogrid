#library(R6)
#library(checkmate)
#library(RCurl)

#' Source Class
#'
#' @description
#' An R6 Class representing a data source with comprehensive validation
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
    initialize = function(source_name,
                          source_version,
                          license,
                          website_url,
                          spatial_extent,
                          temporal_resolution,
                          citation_keys = NULL,
                          aws_bucket = NA_character_,
                          aws_region = NA_character_,
                          download_url = NA_character_,
                          prio_mirror = NA_character_,
                          tags = NULL,
                          reference_keys = NULL) {

      private$input_data <- as.list(environment())

      validation_result <- private$validate_inputs()
      if (!validation_result$valid) {
        stop(validation_result$message)
      }

      private$data$id <- uuid::UUIDgenerate()

      private$data$source_name <- source_name
      private$data$source_version <- source_version
      private$data$license <- license
      private$data$website_url <- website_url
      private$data$spatial_extent <- spatial_extent
      private$data$temporal_resolution <- temporal_resolution
      private$data$citation_keys <- citation_keys
      private$data$aws_bucket <- aws_bucket
      private$data$aws_region <- aws_region
      private$data$prio_mirror <- prio_mirror
      private$data$tags <- tags
      private$data$reference_keys <- reference_keys

      # Handle download URL
      download_result <- private$handle_download_url(download_url)
      private$url_data <- download_result$urls
      private$data$download_url <- download_result$url

      url_validations <- private$validate_urls()
      private$data <- c(private$data, url_validations)

      private$data$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    },

    save_url_file = function() {
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
      cat("Source:", private$data$source_name, "\n")
      cat("Version:", private$data$source_version, "\n")
      cat("License:", private$data$license, "\n")

      if (!is.null(private$data$citation_keys) && nchar(private$data$citation_keys) > 0) {
        citation_keys <- unlist(strsplit(private$data$citation_keys, ";\\s*"))
        tryCatch({
          bib <- RefManageR::ReadBib(bib_path)
          cat("\nCitations:\n")
          for (key in citation_keys) {
            key <- trimws(key)
            if (key %in% names(bib)) {
              citation <- RefManageR::Cite(bib, key)
              cat("- ", citation, "\n", sep = "")
            } else {
              cat("- [Missing citation for key:", key, "]\n")
            }
          }
        }, error = function(e) {
          cat("\nWarning: Could not read bibliography file:", e$message, "\n")
          cat("Citation keys:", private$data$citation_keys, "\n")
        })
      }

      if (!is.null(private$data$reference_keys) && nchar(private$data$reference_keys) > 0) {
        reference_keys <- unlist(strsplit(private$data$reference_keys, ";\\s*"))
        tryCatch({
          bib <- RefManageR::ReadBib(bib_path)
          cat("\nOther references:\n")
          for (key in reference_keys) {
            key <- trimws(key)
            if (key %in% names(bib)) {
              citation <- RefManageR::Cite(bib, key)
              cat("- ", citation, "\n", sep = "")
            } else {
              cat("- [Missing citation for key:", key, "]\n")
            }
          }
        }, error = function(e) {
          cat("\nWarning: Could not read bibliography file:", e$message, "\n")
          cat("Reference keys:", private$data$reference_keys, "\n")
        })
      }

      cat("\nURL Status:\n")
      if (!private$data$website_url_exists) {
        cat("- Warning: Website URL not accessible:", private$data$website_url, "\n")
      }
      if (!is.na(private$data$download_url) && !private$data$download_url_exists) {
        cat("- Warning: Download URL not accessible:", private$data$download_url, "\n")
      }
      if (!is.na(private$data$prio_mirror) && !private$data$prio_mirror_exists) {
        cat("- Warning: PRIO mirror not accessible:", private$data$prio_mirror, "\n")
      }
      if (private$data$website_url_exists &&
          (is.na(private$data$download_url) || private$data$download_url_exists) &&
          (is.na(private$data$prio_mirror) || private$data$prio_mirror_exists)) {
        cat("- All provided URLs are accessible\n")
      }

      cat("\nCreated at:", private$data$created_at, "\n")
      invisible(self)
    }
  ),

  private = list(
    input_data = NULL,
    data = list(),
    url_data = NULL,

    valid_spatial_extents = c(
      "World",
      "Multiple continents",
      "Single continent",
      "Several countries"
    ),

    valid_temporal_resolutions = c(
      "Static",
      "Higher than monthly",
      "Monthly",
      "Quarterly",
      "Yearly",
      "Less than yearly"
    ),

    validate_inputs = function() {
      required_strings <- c(
        "source_name",
        "source_version",
        "license",
        "website_url",
        "spatial_extent",
        "temporal_resolution"
      )

      for (field in required_strings) {
        if (!checkmate::test_string(
          private$input_data[[field]],
          min.chars = 1,
          null.ok = FALSE
        )) {
          return(list(
            valid = FALSE,
            message = sprintf(
              "Invalid %s: Must be a non-empty string. Got: %s",
              field,
              private$input_data[[field]]
            )
          ))
        }
      }

      optional_strings <- c(
        "citation_keys",
        "aws_bucket",
        "aws_region",
        "download_url",
        "prio_mirror",
        "tags",
        "reference_keys"
      )

      for (field in optional_strings) {
        value <- private$input_data[[field]]
        if (!is.null(value) && !is.na(value)) {
          if (!checkmate::test_string(
            value,
            min.chars = 1
          )) {
            return(list(
              valid = FALSE,
              message = sprintf(
                "Invalid %s: Must be NULL, NA, or a non-empty string. Got: %s",
                field,
                value
              )
            ))
          }
        }
      }

      if (!checkmate::test_choice(
        private$input_data$spatial_extent,
        private$valid_spatial_extents
      )) {
        return(list(
          valid = FALSE,
          message = sprintf(
            "Invalid spatial_extent: Must be one of: %s. Got: %s",
            paste(private$valid_spatial_extents, collapse = ", "),
            private$input_data$spatial_extent
          )
        ))
      }

      if (!checkmate::test_choice(
        private$input_data$temporal_resolution,
        private$valid_temporal_resolutions
      )) {
        return(list(
          valid = FALSE,
          message = sprintf(
            "Invalid temporal_resolution: Must be one of: %s. Got: %s",
            paste(private$valid_temporal_resolutions, collapse = ", "),
            private$input_data$temporal_resolution
          )
        ))
      }

      return(list(valid = TRUE, message = NULL))
    },

    handle_download_url = function(url) {
      if (is.na(url) || !is.character(url)) {
        return(list(url = NA, urls = NULL))
      }

      if (file.exists(url) && !dir.exists(url)) {
        tryCatch({
          urls <- readLines(url)
          return(list(url = urls[1], urls = urls))
        }, error = function(e) {
          warning(sprintf("Could not read URL file: %s", e$message))
          return(list(url = NA, urls = NULL))
        })
      }

      return(list(url = url, urls = c(url)))
    },

    save_url_file = function() {
      if (!is.null(private$url_data)) {
        dir.create(file.path("data", "urls"), recursive = TRUE, showWarnings = FALSE)
        fpath <- file.path("data", "urls", paste0(private$data$id, ".txt"))
        writeLines(private$url_data, fpath)
        return(file.path("urls", basename(fpath)))
      }
      return(NA)
    },

    validate_urls = function() {
      results <- list(
        website_url_exists = FALSE,
        download_url_exists = FALSE,
        prio_mirror_exists = FALSE
      )

      tryCatch({
        results$website_url_exists <- RCurl::url.exists(private$data$website_url)
      }, error = function(e) {
        warning(sprintf("Error validating website URL: %s", e$message))
      })

      if (!is.na(private$data$download_url)) {
        tryCatch({
          results$download_url_exists <- RCurl::url.exists(private$data$download_url)
        }, error = function(e) {
          warning(sprintf("Error validating download URL: %s", e$message))
        })
      }

      if (!is.na(private$data$prio_mirror)) {
        tryCatch({
          results$prio_mirror_exists <- RCurl::url.exists(private$data$prio_mirror)
        }, error = function(e) {
          warning(sprintf("Error validating PRIO mirror URL: %s", e$message))
        })
      }

      results
    }
  )
)
