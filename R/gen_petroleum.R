
# Helper functions --------------------------------------------------------
## Note: Will be updated.

prio_year <- function(startyear, endyear){
  purrr::map2(startyear, endyear, `:`)
}


prio_earliest <- function(x){
  x[x < 1946] <- 1946
  x
}


prio_NA <- function(x, na_value){
  x[x == na_value] <- NA
  x
}


# Create yearly petroleum presence dummy ----------------------------------
#' Generate yearly petroleum presence dummy variable. 
#' 
#' @param petro_data PRIO Petroleum Dataset v. 1.2 shapefile 
#' as available from https://www.prio.org/Data/Geographical-and-Resource-Datasets/Petroleum-Dataset/Petroleum-Dataset-v-12/.

prio_petro_y <- function(petro_data){
  # Load data
  petroleum <- sf::st_read(petro_data)
  petroleum <- sf:st_set_crs(petroleum, value = priogrid::prio_crs())

  # Data prep
  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc = DISC, prod = PROD, geometry)

  petroleum$disc <- priogrid::prio_NA(petroleum$disc, -9999)
  petroleum$prod <- priogrid::prio_NA(petroleum$prod, -9999)

  petroleum$disc <- priogrid::prio_earliest(petroleum$disc)
  petroleum$prod <- priogrid::prio_earliest(petroleum$prod)

  
  # Create yearly dummy
  petroleum_y <- petroleum %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(prod) | !is.na(disc)) %>%
    dplyr::mutate(startyear = pmin(prod, disc, na.rm = TRUE), # earliest occurrance of discovery or start of production
                  year = priogrid::prio_year(startyear, 2003),
                  petroleum_y = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::select(gwno, petroleum_y, geometry)
}



# Create static petroleum presence dummy ----------------------------------
#' Generate static petroleum presence dummy variable
#' for records without known year of discovery or production start.
#' 
#' @param petro_data PRIO Petroleum Dataset v. 1.2 shapefile 
#' as available from https://www.prio.org/Data/Geographical-and-Resource-Datasets/Petroleum-Dataset/Petroleum-Dataset-v-12/.


prio_petro_s <- function(petro_data){
  # Load data
  petroleum <- sf::st_read(petro_data)
  petroleum <- sf:st_set_crs(petroleum, value = priogrid::prio_crs())

  # Data prep
  petroleum <- petroleum %>%
    dplyr::select(id = PRIMKEY, gwno = COWCODE, disc = DISC, prod = PROD, geometry)

  petroleum$disc <- priogrid::prio_NA(petroleum$disc, -9999)
  petroleum$prod <- priogrid::prio_NA(petroleum$prod, -9999)
  
  petroleum$disc <- priogrid::prio_earliest(petroleum$disc)
  petroleum$prod <- priogrid::prio_earliest(petroleum$prod)
  
  # Create static dummy
  petroleum_s <- petroleum %>%
    dplyr::filter(is.na(prod) & is.na(disc)) %>%
    dplyr::mutate(petroleum_s = 1) %>%
    dplyr::select(gwno, petroleum_s, geometry)
}


