#' Reads the GDL SHDI dataset
#'
#' @return A tibble with subnational HDI data
#' @export
#'
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
read_gdl_shdi <- function() {
  f <- get_pgfile(
    source_name = "GDL SHDI",
    source_version = "v4.2",
    id = "68b56155-7248-4f6f-b4a7-b1fb13336dae"
  )
  
  df <- readr::read_csv(f, show_col_types = FALSE)
  return(df)
}

