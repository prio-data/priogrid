

# Yearly ------------------------------------------------------------------
#' Generate yearly dummy identifying ongoing large-scale drug cultivation
#' (coca bush, opium poppy and/or cannabis).
#' All data available from http://www.paivilujala.com/drugdata.html.
#'
#' @param cannabis_data Cannabis shapefile from the DRUGDATA dataset.
#' @param coca_data Coca bush shapefile from the DRUGDATA dataset.
#' @param opium_data Opium poppy shapefile from the DRUGDATA dataset.


gen_drugs_y <- function(cannabis_data, coca_data, opium_data){
  # Load three drug datasets
  cannabis <- sf::st_read(cannabis_data, stringsAsFactors = FALSE)
  coca <- sf::st_read(coca_data, stringsAsFactors = FALSE)
  opium <- sf::st_read(opium_data, stringsAsFactors = FALSE)

  # Harmonize variable names and combine drug type tables
  cannabis <- cannabis %>%
    dplyr::select(ID, country = Country_, startyear = Begin_, endyear = End_, geometry)

  coca <- coca %>%
    dplyr::select(ID, country = Country, startyear = Begin_, endyear = End_, geometry)

  opium <- opium %>%
    dplyr::select(ID, country = Country, startyear = Begin, endyear = End, geometry)

  drugs <- rbind(cannabis, coca, opium)

  # Set missing values for endyear to 2002
  drugs$endyear[which(is.na(drugs$endyear))] <- 2002

  # Set earliest startyear to 1946
  drugs$startyear <- priogrid::prio_earliest(drugs$startyear)


  drugs <- drugs %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(year = priogrid::prio_year(startyear, endyear),
           drug_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()

  drugs <- sf::st_set_crs(drugs, value = priogrid::prio_crs())

  drugs <- sf::st_cast(drugs, "MULTIPOLYGON")
}
