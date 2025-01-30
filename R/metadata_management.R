#' Add source to CSV file
#' @param source Source object to add
#' @param csv_file Path to CSV file
#' @return Invisible NULL
add_source <- function(source, csv_file = "data_raw/sources.csv") {
  if (!inherits(source, "Source")) {
    stop("source must be a Source object")
  }

  # Save URL file if exists and update download_url
  url_path <- source$get_url_path()
  if (!is.na(url_path)) {
    source$set_download_url(url_path)
  }

  # Convert to tibble
  source_tibble <- source$to_tibble()

  # Create or append to CSV
  if (!file.exists(csv_file)) {
    readr::write_delim(source_tibble, csv_file, delim = "\t")
  } else {
    readr::write_delim(source_tibble, csv_file, append = TRUE, delim = "\t")
  }

  invisible(NULL)
}
