read_poverty <- function() {
  #f <- get_pgfile(source_name = ",
  #               source_version = "",
  #              id = "93164a9e-089b-467f-b63d-413f8c58d34c")

  poverty <- "/Users/ingvildsmestad/Downloads/Subnational-PovertyEXCEL.xlsx"
  df <- (poverty)
  r <- terra::rast(fname)

  r <- terra::clamp(r, 0, values = FALSE)

  return(r)
}


