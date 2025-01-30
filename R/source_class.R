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

      # Store inputs before validation for error messages
      private$input_data <- as.list(environment())

      # Run validation
      validation_result <- private$validate_inputs()
      if (!validation_result$valid) {
        stop(validation_result$message)
      }

      # Generate UUID for new source
      private$data$id <- uuid::UUIDgenerate()

      # Store validated data
      private$data$source_name <- source_name
      private$data$source_version <- source_version
      private$data$license <- license
      private$data$website_url <- website_url
      private$data$spatial_extent <- spatial_extent
      private$data$temporal_resolution <- temporal_resolution
      private$data$citation_keys <- citation_keys
      private$data$aws_bucket <- aws_bucket
      private$data$aws_region <- aws_region
      private$data$download_url <- download_url
      private$data$prio_mirror <- prio_mirror
      private$data$tags <- tags
      private$data$reference_keys <- reference_keys

      # Add URL validation results
      url_validations <- private$validate_urls()
      private$data <- c(private$data, url_validations)

      # Add timestamp
      private$data$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    },

    #' @description
    #' Convert source to tibble format
    #' @return tibble
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

    #' @description
    #' Print source details
    #' @description
    #' Print source details including full citations
    #' @param bib_path String. Path to REFERENCES.bib file. Defaults to "inst/REFERENCES.bib"
    print = function(bib_path = "inst/REFERENCES.bib") {
      cat("Source:", private$data$source_name, "\n")
      cat("Version:", private$data$source_version, "\n")
      cat("License:", private$data$license, "\n")

      # Handle citations if they exist
      if (!is.null(private$data$citation_keys) && nchar(private$data$citation_keys) > 0) {
        citation_keys <- unlist(strsplit(private$data$citation_keys, ";\\s*"))

        tryCatch({
          # Read bibliography file
          bib <- RefManageR::ReadBib(bib_path)

          # Print each citation
          cat("\nCitations:\n")
          for (key in citation_keys) {
            key <- trimws(key)
            if (key %in% names(bib)) {
              # Format citation using RefManageR
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

      # Print URL statuses
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
    # Store input data for error messages
    input_data = NULL,

    # Store validated data
    data = list(),

    # Valid values for controlled vocabularies
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

    #' Validate all input parameters
    validate_inputs = function() {
      # Required string fields
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

      # Optional string fields
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

      # Validate spatial extent
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

      # Validate temporal resolution
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

      # All validations passed
      return(list(valid = TRUE, message = NULL))
    },

    #' Validate URLs and return existence status
    validate_urls = function() {
      results <- list(
        website_url_exists = FALSE,
        download_url_exists = FALSE,
        prio_mirror_exists = FALSE
      )

      # Validate website URL
      tryCatch({
        results$website_url_exists <- RCurl::url.exists(private$data$website_url)
      }, error = function(e) {
        warning(sprintf("Error validating website URL: %s", e$message))
      })

      # Validate download URL if provided
      if (!is.na(private$data$download_url)) {
        tryCatch({
          results$download_url_exists <- RCurl::url.exists(private$data$download_url)
        }, error = function(e) {
          warning(sprintf("Error validating download URL: %s", e$message))
        })
      }

      # Validate PRIO mirror if provided
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
