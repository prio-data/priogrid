# Functions that use or improve sf features, handling
# geometry data.


# The stuff below can be done for any monthly cross-section, but we should come
# up with a smarter algorithm so that we do not need to calculate everything
# anew every month.
#### I have just started here with some code that could be a smarter algorithm...
compare_crossection <- function(crossection_date, cshp){
  if(crossection_date - months(1) < min(cshp$startdate)){
    res <- tibble::tibble(date = crossection_date, change = 1)
  } else {
    past_crossection <- sf::st_combine(cshp[(crossection_date - months(1)) %within% cshp$date_interval,])
    cshp_crossection <- sf::st_combine(cshp[crossection_date %within% cshp$date_interval,])

    if(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0, sparse = F) ){
      res <- tibble::tibble(date = crossection_date, change = 0)
    } else{
      res <- tibble::tibble(date = crossection_date, change = 1)
    }

  }
  res
}
