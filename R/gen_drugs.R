

# Yearly ------------------------------------------------------------------

#' Generate drug dummy.
#' 
#' Generate yearly dummy identifying ongoing large-scale drug cultivation
#' (coca bush, opium poppy and/or cannabis).
#'
#' @param cannabis_data Cannabis shapefile from the DRUGDATA dataset.
#' @param coca_data Coca bush shapefile from the DRUGDATA dataset.
#' @param opium_data Opium poppy shapefile from the DRUGDATA dataset.


gen_drug_y <- function(cannabis_data, coca_data, opium_data){
  drugs <- priogrid::prep_drugs(cannabis_data, coca_data, opium_data)
  
  drugs <- drugs %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(year = priogrid::prio_year(begin, end),
                  drug_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
  
  drugs <- sf::st_cast(drugs, "MULTIPOLYGON")
  
  drugs <- priogrid::yearly_stack(drugs, variable = "drug_y", raster.fun = "first")
  
}




# Drug data prep function -------------------------------------------------


prep_drugs <- function(cannabis_data, coca_data, opium_data){
  # Load three drug datasets
  cannabis <- sf::st_read(cannabis_data, stringsAsFactors = FALSE)
  coca <- sf::st_read(coca_data, stringsAsFactors = FALSE)
  opium <- sf::st_read(opium_data, stringsAsFactors = FALSE)
  
  cleanup <- function(data, variables){
    data <- data %>%
      janitor::clean_names() %>%
      dplyr::select(variables)  
    }
  variables <- c("id", "country", "begin", "end", "geometry")
  
  drugs <- rbind(cleanup(cannabis, variables),
                 cleanup(coca, variables),
                 cleanup(opium, variables))
  
  drugs$end[which(is.na(drugs$end))] <- 2002
  
  drugs$begin <- priogrid::prio_earliest(drugs$begin)
  
  drugs <- sf::st_set_crs(drugs, value = priogrid::prio_crs())
  
  drugs
  
}