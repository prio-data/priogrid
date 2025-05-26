#' Read the GDL SHDI data
#'
#' Reads the downloaded SHDI dataset from GlobalDataLab.
#'
#' @return A tibble with SHDI data.
#' @export
#'
#' @references
#' \insertRef{globaldatalabSubnationalHumanDevelopment2019}{priogrid}
read_shdi <- function() {
  f <- get_pgfile(
    source_name = "GDL SHDI",
    source_version = "v4.2",
    id = "1c40c405-b540-4bde-ae4a-84f5d52ecac7"  
  )
  df <- readr::read_csv(f, show_col_types = FALSE)
  return(df)
}
