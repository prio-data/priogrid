cshapes_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))

#' Reads the CShapes 2.0 raw data
#'
#' It correctly formats gwsdate and gwedate as date objects, and adds a utility column date_interval.
#'
#' @return an object of class sf
#' @export
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
read_cshapes <- function(){
  f <- get_pgfile(source_name = "ETH ICR cShapes",
                  source_version = "2.0",
                  id = "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
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

#' Share of grid-cell intersecting with the international state system (cShapes 2.0)
#'
#' Takes the cShapes data and returns
#' a raster-mask that is true for the grid cells that intersects with country borders
#' included in the international state system at the measurement_date
#'
#' @param measurement_date A single date as Date object
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#'
#' @export
#' @examples
#' # cshapes_cover_share_one_cross_section <- cshapes_cover_share(as.Date("2010-01-01"))
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
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
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#'
#' @export
#' @examples
#' # cshapes_cover_one_cross_section <- cshapes_cover(as.Date("2010-01-01"))
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
cshapes_cover <- function(measurement_date, min_cover = 0, cshp = read_cshapes()){
  cshp_cover <- cshapes_cover_share(measurement_date, cshp)

  cshp_cover <- terra::ifel(cshp_cover < min_cover, NA, cshp_cover)

  pg <- prio_blank_grid()
  res <- terra::intersect(cshp_cover, pg)
  names(res) <- "cshapes_cover"
  return(res)
}

#' Generate cshapes_cover_share variable
#'
#' The variable can be generated with up to daily temporal resolution.
#'
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # cshapes_cover_share <- gen_cshapes_cover_share()
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
gen_cshapes_cover_share <- function(cshp = read_cshapes()){
  time_slices <- pg_dates()
  temporal_interval <- lubridate::interval(min(cshp$gwsdate), max(cshp$gwedate))
  time_slices <- time_slices[time_slices %within% temporal_interval]

  r <- cshapes_cover_share(time_slices[1], cshp = cshp)
  for(i in 2:length(time_slices)){
    t <- time_slices[i]
    terra::add(r) <- cshapes_cover_share(t, cshp = cshp)
  }
  names(r) <- as.character(time_slices)
  r
}

#' The Gleditsch-Ward (cShapes 2.0 version) code in each grid-cell
#'
#' Uses max area for cells with multiple countries. If actual tie, the algorithm will use the lowest index.
#'
#' @param measurement_date A single date as Date object
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # gwcode_one_cross_section <- cshapes_gwcode(as.Date("2010-01-01"))
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
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


#' Generate cshapes_gwcode variable
#'
#' The variable can be generated with up to daily temporal resolution.
#'
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # gwcode <- gen_cshapes_gwcode()
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
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


#' Calculates bdist2 variable for a single time slice.
#'
#' bdist2 gives the spherical distance (in kilometer) from the cell centroid to
#' the border of the nearest neighboring country, regardless of whether the nearest
#' country is located across international waters. Based on cShapes dataset.`
#'
#' The variable can be generated with up to daily temporal resolution. It does
#' take quite a long time to estimate, but can use a past result if there are
#' no changes in borders.
#'
#' @param measurement_date A single date as Date object
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#' @param past_result A past result of bdist2.
#'
#' @return list with SpatRast with bdist2, and Sf objects with country boundaries and shared_borders.
#' @export
#'
#' @examples
#' # bdist2 <- bdist2(as.Date("2010-01-01"))
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
bdist2 <- function(measurement_date, cshp = read_cshapes(), past_result = NULL){
  pg <- prio_blank_grid()

  features <- cshp |> dplyr::filter(measurement_date %within% date_interval)
  boundaries <- sf::st_boundary(features)
  boundaries <- sf::st_cast(boundaries, "MULTILINESTRING")

  if(!is.null(past_result)){
    no_changes <- sf::st_equals(boundaries |> sf::st_combine(), past_result$boundaries |> sf::st_combine(), sparse = FALSE)
    if(no_changes){
      return(past_result)
    }
  }

  sf::sf_use_s2(TRUE)
  shared_borders <- list()
  for(i in 1:nrow(boundaries)){
    shared_borders[[i]] <- sf::st_intersection(boundaries[i,], boundaries[-i,])
  }
  shared_borders <- dplyr::bind_rows(shared_borders)
  #shared_borders |> sf::st_geometry() |> plot()

  res <- terra::distance(pg, shared_borders |> sf::st_combine() |> terra::vect(), rasterize = TRUE)

  cover <- cshapes_cover(measurement_date, cshp = cshp)
  values(cover) <- dplyr::if_else(values(cover) == T, 1, NA)

  return(list("bdist2" = res*cover, "boundaries" = boundaries, "shared_borders" = shared_borders))
}

#' Generate bdist2 variable
#'
#' bdist2 gives the spherical distance (in kilometer) from the cell centroid to
#' the border of the nearest neighboring country, regardless of whether the nearest
#' country is located across international waters. Based on cShapes dataset.`
#'
#' The variable can be generated with up to daily temporal resolution. It does
#' take quite a long time to estimate, but uses a past result if there are
#' no changes in borders.
#'
#' @param cshp The CShapes dataset, for instance as given by [read_cshapes()]
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # bdist2 <- gen_bdist2()
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
gen_bdist2 <- function(cshp = read_cshapes()){
  temporal_interval <- lubridate::interval(min(cshp$gwsdate), max(cshp$gwedate))
  time_slices <- pg_dates()
  time_slices <- time_slices[time_slices %within% temporal_interval]

  res <- bdist2(time_slices[1], cshp = cshp)
  r <- res$bdist2
  for(i in 2:length(time_slices)){
    print(i)
    t <- time_slices[i]
    res <- bdist2(t, cshp = cshp, past_result = res)
    terra::add(r) <- res$bdist2
  }
  names(r) <- as.character(time_slices)
  r
}
