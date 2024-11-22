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
cshapes_cover_share <- function(measurement_date, cshp = read_cshapes()){
  assertthat::assert_that(lubridate::is.Date(measurement_date))

  pg <- prio_blank_grid()
  cs <- cshp |> dplyr::filter(measurement_date %within% date_interval)
  #cshp_cover <- terra::rasterize(terra::vect(cs), pg, fun = "min", cover = T)

  cs_combined <- cs |> dplyr::summarize(geometry = sf::st_combine(geometry))
  coversh <- exactextractr::exact_extract(pg, cs_combined)

  ra <- exactextractr::rasterize_polygons(cs_combined, pg)
  pg <- pg*ra # Remove non-land cells

  res <- terra::classify(pg, coversh[[1]])



  names(res) <- "cshapes_cover_share"
  return(res)
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
cshapes_cover <- function(measurement_date, min_cover = 0, cshp = read_cshapes()){
  cshp_cover <- cshapes_cover_share(measurement_date, cshp)

  cshp_cover <- terra::ifel(cshp_cover < min_cover, NA, cshp_cover)

  pg <- prio_blank_grid()
  res <- terra::intersect(cshp_cover, pg)
  names(res) <- "cshapes_cover"
  return(res)
}

#' The Gleditsch-Ward (cShapes 2.0 version) code in each grid-cell
#'
#' Uses max area for cells with multiple countries. If actual tie, the algorithm will use the lowest index.
#'
#' @param measurement_date A single date as Date object
#' @param cshp The CShapes dataset, for instance as given by [priogrid::read_cshapes()]
#'
#' @return
#' @export
#'
#' @examples
cshapes_gwcode <- function(measurement_date, cshp = read_cshapes()){
  pg <- prio_blank_grid()
  cs <- cshp |> dplyr::filter(measurement_date %within% date_interval)
  res <- exactextractr::rasterize_polygons(cs, pg)
  cmat <- cbind(terra::minmax(res)[1,]:terra::minmax(res)[2,], cs$gwcode)
  res <- terra::classify(res, cmat)

  represented_gwcodes <- terra::values(res) |> as.vector() |> unique()
  countries_not_included <- cs$gwcode[!cs$gwcode %in% represented_gwcodes]
  assertthat::assert_that(length(countries_not_included)== 0)
  # res <- terra::as.factor(res)

  # Still need to add provision for countries that are minorities the cells
  # they occupy (Palestine would be an example).
  res
}


gen_cshapes_gwcode <- function(cshp = read_cshapes()){
  time_slices <- pg_dates()
  temporal_interval <- lubridate::interval(min(cshp$gwsdate), max(cshp$gwedate))
  time_slices <- time_slices[time_slices %within% temporal_interval]

  r <- cshapes_gwcode(time_slices[1], cshp = cshp)
  for(i in 2:length(time_slices)){
    t <- time_slices[i]
    terra::add(r) <- cshapes_gwcode(t, cshp = cshp)
  }
  names(r) <- as.character(time_slices)
  r
}
