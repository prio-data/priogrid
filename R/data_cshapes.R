cshapes_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))

#' Reads the CShapes 2.0 raw data
#'
#' It correctly formats gwsdate and gwedate as date objects, and adds a utility column date_interval.
#'
#' @return an object of class sf
#' @export
read_cshapes <- function(){
  f <- get_pgfile("CShapes", "2.0")
  df <- sf::read_sf(f) # CShapes comes in GeoJSON format
  df <- df |>
    dplyr::mutate(
      gwsdate = as.Date(gwsdate, format = "%d.%m.%Y %H:%M:%S"),
      gwedate = as.Date(gwedate, format = "%d.%m.%Y %H:%M:%S")) |>
    dplyr::mutate(
      date_interval = lubridate::interval(gwsdate, gwedate)
    )
  return(df)
}

#' Finds the areas and dates where borders have changed in the cshapes dataset.
#'
#' @param cshp The CShapes dataset, for instance as given by [priogrid::read_cshapes()]
#'
#' @return a list, one sf df for each crossection
cshapes_changed_areas_base <- function(cshp){
  compare_crossection <- function(crossection_date, cshp, dates_with_changes){
    message(crossection_date)
    if(which(crossection_date == dates_with_changes) == 1){
      changed_areas <- cshp[crossection_date %within% cshp$date_interval,]
      changed_areas$crossection_date <- crossection_date
    } else {
      last_date <- dates_with_changes[which(crossection_date == dates_with_changes) - 1]
      past_crossection <- cshp[last_date %within% cshp$date_interval,]
      cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]
      new_changes <- lengths(sf::st_equals_exact(cshp_crossection, past_crossection, par = 0)) == 0

      if(any(new_changes)){
        cshp_crossection$changes <- new_changes
        cshp_crossection <- dplyr::filter(cshp_crossection, changes)
        # Combine and buffer to make sure area-calculations are done again for bordering cells. st_union to check validity.
        geometry <- sf::st_union(sf::st_buffer(sf::st_combine(cshp_crossection), 1))
        changed_areas <- sf::st_sf(geometry)
        changed_areas$crossection_date <- crossection_date

      } else{
        changed_areas <- NULL
      }
    }
    return(changed_areas)
  }

  use_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)

  dates_with_changes <- sort(unique(unique(cshp$gwsdate), unique(cshp$gwsdate)))

  #Find all areas and dates where there have been changes since last day.
  changed_areas <- lapply(dates_with_changes, compare_crossection, cshp = cshp, dates_with_changes = dates_with_changes)

  changed_areas <- parallel::mclapply(dates_with_changes, compare_crossection, cshp, dates_with_changes)
  changed_areas[sapply(changed_areas, is.null)] <- NULL

  crossection_dates <- sapply(changed_areas, function(x) unique(x$crossection_date))
  crossection_dates <- as.character(as.Date(crossection_dates, origin = as.Date("1970-1-1")))

  names(changed_areas) <- crossection_dates

  sf::sf_use_s2(use_s2)
  return(changed_areas)
}

#' @describeIn cshapes_changed_areas_base Finds the areas and dates where borders have changed in the cshapes dataset. Memoise/caching wrapper.
#'
#' @export
cshapes_changed_areas <- memoise::memoise(cshapes_changed_areas_base, cache = cshapes_cache)

#' Share of grid-cell intersecting with the international state system (cShapes 2.0)
#'
#' Takes the cShapes data and returns
#' a raster-mask that is true for the grid cells that intersects with country borders
#' included in the international state system at the measurement_date
#'
#' @param measurement_date A single date as Date object
#' @param cshp The CShapes dataset, for instance as given by [priogrid::read_cshapes()]
#'
#' @export
gen_cshapes_cover_share <- function(measurement_date, cshp = read_cshapes()){
  assertthat::assert_that(lubridate::is.Date(measurement_date))

  pg <- prio_blank_grid()
  cs <- cshp |> dplyr::filter(measurement_date %within% date_interval)
  cshp_cover <- terra::rasterize(terra::vect(cs), pg, fun = "min", cover = T)

  names(cshp_cover) <- "cshapes_cover_share"
  return(cshp_cover)
}

#' Whether or not a grid-cell intersects with the international state system (cShapes 2.0)
#'
#' Takes the cShapes data and returns
#' a raster-mask that is true for the grid cells that intersects with country borders
#' included in the international state system at the measurement_date
#'
#' @param measurement_date A single date as Date object
#' @param cshp The CShapes dataset, for instance as given by [priogrid::read_cshapes()]
#'
#' @export
gen_cshapes_cover <- function(measurement_date, cshp = read_cshapes()){
  cshp_cover <- gen_cshapes_cover_share(measurement_date, cshp)
  pg <- prio_blank_grid()
  res <- terra::intersect(cshp_cover, pg)
  names(res) <- "cshapes_cover"
  return(res)
}
