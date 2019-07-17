# Helper functions --------------------------------------------------------
### Some simplifying functions used in functions generating resource variables
# To be updated


## Create time series
prio_year <- function(startyear, endyear){
  purrr::map2(startyear, endyear, `:`)
}

## Set earliest start year to 1946
prio_earliest <- function(x){
  x[x < 1946] <- 1946
  x
}

## Set NA values
prio_NA <- function(x, na_value){
  x[x == na_value] <- NA
  x
}


## Generate yearly dummy for resource variables
# NB! Assign variable name manually by piping dplyr::rename(new_name = dummy) after function

yearly_dummy <- function(data, id, disc.year, prod.year, endyear){
  data <- data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(disc.year) | !is.na(prod.year)) %>%
    dplyr::mutate(startyear = pmin(disc.year, prod.year, na.rm = TRUE),
                  year = priogrid::prio_year(startyear, endyear),
                  dummy = 1) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}



## Generate static dummy for resource variables
# NB! Assign variable name manually by piping dplyr::rename(new_name = dummy) after function

static_dummy <- function(data, disc.year, prod.year){
  data <- data %>%
    dplyr::filter(is.na(disc.year) & is.na(prod.year)) %>%
    dplyr::mutate(dummy = 1) %>%
    dplyr::select(gwno, dummy, geometry) # not sure whether to include select() 
  
}


