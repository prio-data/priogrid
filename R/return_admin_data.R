#!/usr/bin/env Rscript

library(httr)
library(jsonlite)

# Standalone function
search_admin_layer <- function(country_query, admin_level = 0) {

  # Validate admin_level
  admin_level <- as.character(admin_level)
  if (!admin_level %in% c("0", "1", "2", "3", "4", "5")) {
    stop("Invalid admin level. Must be 0, 1, 2, 3, 4, or 5.")
  }

  # Build API query
  base_url <- "https://data.humdata.org/api/3/action/package_search"
  query_params <- list(q = paste0(country_query, " administrative boundaries"))

  # Request
  response <- GET(base_url, query = query_params)
  response_content <- content(response, as = "text")
  response_json <- fromJSON(response_content, flatten = TRUE)

  # Datasets
  datasets <- response_json$result$results

  if (nrow(datasets) == 0) {
    cat("No datasets found for this country.\n")
    quit(status = 1)
  }

  # Collect all available resources
  resources_all <- list()
  for (i in seq_len(nrow(datasets))) {
    resources <- datasets$resources[[i]]
    for (j in seq_len(nrow(resources))) {
      resources_all <- append(resources_all, list(list(
        name = resources$name[j],
        format = resources$format[j],
        url = resources$url[j]
      )))
    }
  }

  # Find matching admin level
  matching_resources <- Filter(function(res) grepl(paste0("ADM", admin_level), res$name, ignore.case = TRUE), resources_all)

  if (length(matching_resources) == 0) {
    cat("\n❗ No resources found at Admin Level", admin_level, "for", country_query, ".\n")

    # Suggest available admin levels
    admin_levels_available <- unique(unlist(lapply(resources_all, function(res) {
      m <- regmatches(res$name, regexpr("ADM[0-9]", res$name, ignore.case = TRUE))
      if (length(m) > 0) return(m) else return(NULL)
    })))

    if (length(admin_levels_available) > 0) {
      cat("Available admin levels found:\n")
      cat(paste(admin_levels_available, collapse = ", "), "\n")
    } else {
      cat("No admin levels detected in available datasets.\n")
    }
    quit(status = 1)
  }

  # Look for SHP
  shp_resources <- Filter(function(res) tolower(res$format) == "shp", matching_resources)

  if (length(shp_resources) > 0) {
    cat("\n✅ .shp file available for Admin Level", admin_level, ":\n")
    for (res in shp_resources) {
      cat(paste0("  - ", res$name, "\n"))
      cat(paste0("    URL: ", res$url, "\n"))
    }
  } else {
    cat("\n⚠️ No .shp file found for Admin Level", admin_level, ". Available formats:\n")
    for (res in matching_resources) {
      cat(paste0("  - ", res$name, " (", res$format, ")\n"))
      cat(paste0("    URL: ", res$url, "\n"))
    }
  }
}

# ---- Handle command-line arguments ----

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  cat("Usage: Rscript search_admin.R \"Country Name\" admin_level (0,1,2,3,4,5)\n")
  quit(status = 1)
}

country_query <- args[1]
admin_level <- args[2]

search_admin_layer(country_query, admin_level)
