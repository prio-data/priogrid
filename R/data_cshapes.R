cshapes_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))

#' Reads the CShapes 2.0 raw data
#'
#' Downloads and processes CShapes 2.0 historical country boundaries data.
#' The function formats date columns and adds utility columns for temporal
#' analysis compatible with PRIO-GRID.
#'
#' @details
#' CShapes 2.0 provides historical country boundary data from 1886 onwards
#' at high spatial resolution. This function:
#' \itemize{
#'   \item Downloads the CShapes 2.0 dataset if not already cached
#'   \item Converts gwsdate and gwedate columns to proper Date objects
#'   \item Adds a date_interval utility column for temporal operations
#'   \item Returns data in sf format for spatial operations
#' }
#'
#' @return A \code{sf} object containing historical country boundaries with:
#'   \itemize{
#'     \item Geometry: Polygon/multipolygon country boundaries
#'     \item gwsdate, gwedate: Properly formatted Date objects for country periods
#'     \item date_interval: Utility column for temporal analysis
#'     \item Coordinate reference system: WGS84 (EPSG:4326)
#'   }
#'
#' @seealso
#' \code{\link{pg_dates}} for PRIO-GRID temporal coverage,
#' \code{\link[sf]{st_sf}} for sf object details
#'
#' @examples
#' \dontrun{
#' # Read CShapes 2.0 data
#' cshapes_data <- read_cshapes()
#'
#' # Examine the structure
#' print(cshapes_data)
#'
#' # Check date formatting
#' head(cshapes_data$gwsdate)
#' head(cshapes_data$gwedate)
#'
#' # View available columns
#' names(cshapes_data)
#'
#' # Filter to specific time period
#' modern_boundaries <- cshapes_data[cshapes_data$gwsdate >= as.Date("2000-01-01"), ]
#'
#' # Plot boundaries for a specific period
#' plot(sf::st_geometry(modern_boundaries), main = "Country Boundaries (2000+)")
#' }
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

#' Calculate Grid Cell Coverage by International State System Boundaries (cShapes 2.0)
#'
#' Computes the proportion of each PRIO-GRID cell that intersects with country
#' boundaries from the CShapes 2.0 dataset at a specified date. The function
#' performs spatial overlay analysis to determine how much of each grid cell
#' falls within internationally recognized state boundaries.
#'
#' @details
#' This function uses CShapes 2.0 historical boundary data to calculate the
#' proportion of each PRIO-GRID cell covered by countries that were part of
#' the international state system at the measurement date. The function:
#' \itemize{
#'   \item Filters CShapes data to boundaries valid at the measurement date
#'   \item Performs exact area-weighted extraction using the exactextractr package
#'   \item Returns proportional coverage values (0-1) for each grid cell
#'   \item Masks out non-land areas from the final result
#' }
#'
#' The coverage values represent the fraction of each PRIO-GRID cell area that
#' falls within internationally recognized state boundaries.
#'
#' @param measurement_date A single \code{Date} object specifying the date for
#'   which to calculate boundary coverage. Must be within the temporal range
#'   of the CShapes dataset (1886 onwards).
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data with
#'   properly formatted date intervals. Defaults to \code{\link{read_cshapes}()}
#'   if not provided.
#'
#' @seealso
#' \code{\link{read_cshapes}} for loading CShapes boundary data,
#' \code{\link{prio_blank_grid}} for PRIO-GRID structure,
#' \code{\link[exactextractr]{exact_extract}} for area-weighted extraction
#'
#' @examples
#' \dontrun{
#' # Calculate state system coverage for 2010
#' coverage_2010 <- cshapes_cover_share(as.Date("2010-01-01"))
#'
#' # View the result
#' print(coverage_2010)
#'
#' # Plot the coverage
#' terra::plot(coverage_2010,
#'             main = "cShapes coverage 2010",
#'             col = terrain.colors(100))
#'
#' # Calculate coverage for different time periods
#' coverage_1950 <- cshapes_cover_share(as.Date("1950-01-01"))
#' coverage_2000 <- cshapes_cover_share(as.Date("2000-01-01"))
#'
#' # Compare coverage over time
#' coverage_change <- coverage_2000 - coverage_1950
#' terra::plot(coverage_change,
#'             main = "Change in cShapes coverage 1950-2000")
#'
#' }
#'
#' @export
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


#' Binary Mask for Grid Cells Intersecting International State System (cShapes 2.0)
#'
#' Creates a binary raster mask indicating whether PRIO-GRID cells intersect
#' with country boundaries from the international state system (CShapes 2.0)
#' at a specified date, with optional minimum coverage threshold.
#'
#' @details
#' This function builds on \code{\link{cshapes_cover_share}} to create a binary
#' classification of grid cells. It:
#' \itemize{
#'   \item Calculates proportional coverage using CShapes boundary data
#'   \item Applies a minimum coverage threshold (cells below threshold become NA)
#'   \item Returns a binary mask where non-NA values indicate state system intersection
#' }
#'
#' @param measurement_date A single \code{Date} object specifying the date for
#'   boundary analysis. Must be within CShapes temporal coverage.
#' @param min_cover Numeric. Minimum coverage threshold (0-1). Grid cells with
#'   state coverage below this value are set to NA. Default is 0 (any intersection).
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data.
#'   Defaults to \code{\link{read_cshapes}()} if not provided.
#'
#' @examples
#' \dontrun{
#' # Binary mask for any state system intersection in 2010
#' state_mask_2010 <- cshapes_cover(as.Date("2010-01-01"))
#'
#' # Require at least 25% coverage
#' substantial_state <- cshapes_cover(as.Date("2010-01-01"), min_cover = 0.25)
#' }
#' @export
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
#' Creates a multi-layer raster containing state system coverage proportions
#' for all PRIO-GRID temporal slices. The function generates time series data
#' showing how grid cell coverage by international state boundaries changes
#' over time using CShapes 2.0 boundary data.
#'
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data.
#'   Defaults to \code{\link{read_cshapes}()} if not provided.
#'
#' @return A \code{SpatRaster} object
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate full temporal coverage data
#' temporal_coverage <- gen_cshapes_cover_share()
#'
#' # Examine the structure
#' print(temporal_coverage)
#' }
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

#' Assign Gleditsch-Ward Country Codes to PRIO-GRID Cells
#'
#' Determines the dominant country in each PRIO-GRID cell using CShapes 2.0
#' boundary data and assigns the corresponding Gleditsch-Ward country code.
#' For cells containing multiple countries, the country with the largest area
#' coverage is assigned, with ties broken by selecting the lowest country code.
#'
#' @param measurement_date A single \code{Date} object specifying the date for
#'   country assignment. Must be within the temporal range of the CShapes dataset.
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data with
#'   Gleditsch-Ward country codes. Defaults to \code{\link{read_cshapes}()} if not provided.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Small countries or territories may not appear if they don't dominate any grid cells
#'   \item The function includes an assertion check to verify all countries are represented
#'   \item Future versions may include provisions for minority country representation
#' }
#'
#' @examples
#' \dontrun{
#' # Assign country codes for 2010
#' country_codes_2010 <- cshapes_gwcode(as.Date("2010-01-01"))
#'
#' print(country_codes_2010)
#' }
#' @export
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
#' Creates a multi-layer raster containing Gleditsch-Ward country codes for
#' all PRIO-GRID temporal slices. The function generates time series data
#' showing how country assignments to grid cells change over time due to
#' territorial changes, state formation, and dissolution events.
#'
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data with
#'   Gleditsch-Ward country codes and temporal information. Defaults to
#'   \code{\link{read_cshapes}()} if not provided.
#'
#' @return A \code{SpatRaster} object
#'
#' @examples
#' \dontrun{
#' # Generate full temporal country code dataset
#' temporal_gwcodes <- gen_cshapes_gwcode()
#'
#' print(temporal_gwcodes)
#' }
#' @export
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

#' Calculate distance to nearest land-contiguous border (bdist1)
#'
#' Computes the spherical distance (in kilometers) from each PRIO-GRID cell centroid
#' to the border of the nearest land-contiguous neighboring country using CShapes 2.0
#' boundary data. This implies that cells in e.g. Northern Denmark are measured to the
#' border to Germany even if the straight-line distance to Norway (across international waters)
#' is shorter. Cells belonging to island states with no contiguous neighboring country
#' (e.g., New Zealand) are coded as missing. Islands within states are still measured.
#'
#' The function includes optimization logic that reuses previous calculations
#' if country boundaries haven't changed since the last computation, significantly
#' reducing processing time for temporal sequences.
#'
#' Distance calculations use spherical geometry (S2) for accurate measurements
#' across the globe, particularly important for high-latitude regions.
#'
#' @param measurement_date A single \code{Date} object specifying the date for
#'   boundary analysis. Must be within CShapes temporal coverage.
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data.
#'   Defaults to \code{\link{read_cshapes}()} if not provided.
#' @param past_result A list object from a previous \code{bdist1} calculation.
#'   If boundaries haven't changed, the function returns this result directly,
#'   avoiding recomputation. Default is NULL.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item \code{bdist1}: A \code{SpatRaster} with distances (km) from cell centroids
#'       to nearest international borders, masked to state system coverage
#'     \item \code{boundaries}: An \code{sf} object with country boundary line geometries
#'   }
#'
#' @examples
#' \dontrun{
#' # Calculate border distances for 2010
#' border_dist_2010 <- bdist1(as.Date("2010-01-01"))
#' }
#' @export
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
bdist1 <- function(measurement_date, cshp = read_cshapes(), past_result = NULL){
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

  gwcodes <- cshapes_gwcode(measurement_date)
  bdist1 <- list()
  for(i in 1:length(shared_borders)){
    if(nrow(shared_borders[[i]]) > 0){
      gwcode <- shared_borders[[i]]$gwcode |> unique()
      gwrast <- ifel(gwcodes == gwcode, 1, NA)
      tmp  <- terra::distance(gwcodes, shared_borders[[i]] |> sf::st_combine() |> terra::vect(), rasterize = TRUE)
      bdist1[[as.character(gwcode)]] <- tmp * gwrast
    }
  }
  bdist1 <- terra::mosaic(terra::sprc(bdist1))

  return(list("bdist1" = bdist1, "boundaries" = boundaries))
}

#' Generate distance to nearest land-contiguous border over time (bdist1)
#'
#' Creates a multi-layer raster containing distances
#' to the border of the nearest land-contiguous neighboring country using CShapes 2.0
#' boundary data for temporal time slices as defined in PRIOGRID.
#' This implies that cells in e.g. Northern Denmark are measured to the
#' border to Germany even if the straight-line distance to Norway (across international waters)
#' is shorter. Cells belonging to island states with no contiguous neighboring country
#' (e.g., New Zealand) are coded as missing. Islands within states are still measured.
#'
#' The function automatically uses past results from previous time slices to
#' reduce computation time when country boundaries remain unchanged
#' between consecutive periods.
#'
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data with
#'   temporal information. Defaults to \code{\link{read_cshapes}()} if not provided.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item This function is computationally intensive and may take hours to complete
#'   \item Progress indicators are printed during processing (time slice numbers)
#'   \item The optimization using past results significantly reduces total computation time
#'   \item Consider running in segments for very long time series to manage memory usage
#' }
#'
#' @examples
#' \dontrun{
#' # Generate full temporal border distance dataset
#' # Warning: This may take several hours to complete
#' temporal_bdist1 <- gen_bdist1()
#'
#' print(temporal_bdist1)
#' }
#' @export
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
gen_bdist1 <- function(cshp = read_cshapes()){
  temporal_interval <- lubridate::interval(min(cshp$gwsdate), max(cshp$gwedate))
  time_slices <- pg_dates()
  time_slices <- time_slices[time_slices %within% temporal_interval]

  res <- bdist1(time_slices[1], cshp = cshp)
  r <- res$bdist1
  for(i in 2:length(time_slices)){
    print(i)
    t <- time_slices[i]
    res <- bdist1(t, cshp = cshp, past_result = res)
    terra::add(r) <- res$bdist1
  }
  names(r) <- as.character(time_slices)
  r
}


#' Calculate distance to nearest international border (bdist2)
#'
#' Computes the spherical distance (in kilometers) from each PRIO-GRID cell
#' centroid to the nearest international border using CShapes 2.0 boundary data.
#' The function calculates distances to shared borders between countries,
#' regardless of whether countries are separated by international waters.
#'
#' The function includes optimization logic that reuses previous calculations
#' if country boundaries haven't changed since the last computation, significantly
#' reducing processing time for temporal sequences.
#'
#' Distance calculations use spherical geometry (S2) for accurate measurements
#' across the globe, particularly important for high-latitude regions.
#'
#' @param measurement_date A single \code{Date} object specifying the date for
#'   boundary analysis. Must be within CShapes temporal coverage.
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data.
#'   Defaults to \code{\link{read_cshapes}()} if not provided.
#' @param past_result A list object from a previous \code{bdist2} calculation.
#'   If boundaries haven't changed, the function returns this result directly,
#'   avoiding recomputation. Default is NULL.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item \code{bdist2}: A \code{SpatRaster} with distances (km) from cell centroids
#'       to nearest international borders, masked to state system coverage
#'     \item \code{boundaries}: An \code{sf} object with country boundary line geometries
#'     \item \code{shared_borders}: An \code{sf} object with shared border segments
#'       between neighboring countries
#'   }
#'
#' @examples
#' \dontrun{
#' # Calculate border distances for 2010
#' border_dist_2010 <- bdist2(as.Date("2010-01-01"))
#' }
#' @export
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

#' Generate multi-temporal distance to international borders data (bdist2)
#'
#' Creates a multi-layer raster containing distances to nearest international
#' borders for all PRIO-GRID temporal slices. The function generates time series
#' data showing how proximity to international boundaries changes over time due
#' to territorial changes, state formation, and dissolution events.
#'
#' The function automatically uses past results from previous time slices to
#' dramatically reduce computation time when country boundaries remain unchanged
#' between consecutive periods. This makes temporal sequence generation much
#' more efficient than calculating each period independently.
#'
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data with
#'   temporal information. Defaults to \code{\link{read_cshapes}()} if not provided.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item This function is computationally intensive and may take hours to complete
#'   \item Progress indicators are printed during processing (time slice numbers)
#'   \item The optimization using past results significantly reduces total computation time
#'   \item Consider running in segments for very long time series to manage memory usage
#' }
#'
#' @examples
#' \dontrun{
#' # Generate full temporal border distance dataset
#' # Warning: This may take several hours to complete
#' temporal_bdist2 <- gen_bdist2()
#'
#' print(temporal_bdist2)
#' }
#' @export
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

#' Calculate nearest distance to a country's own borders (bdist3)
#'
#' Computes the spherical distance (in kilometers) from each PRIO-GRID cell centroid
#' to the territorial outline of the country the cell belongs to using CShapes 2.0
#' boundary data.
#'
#' The function includes optimization logic that reuses previous calculations
#' if country boundaries haven't changed since the last computation, significantly
#' reducing processing time for temporal sequences.
#'
#' Distance calculations use spherical geometry (S2) for accurate measurements
#' across the globe, particularly important for high-latitude regions.
#'
#' @param measurement_date A single \code{Date} object specifying the date for
#'   boundary analysis. Must be within CShapes temporal coverage.
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data.
#'   Defaults to \code{\link{read_cshapes}()} if not provided.
#' @param past_result A list object from a previous \code{bdist3} calculation.
#'   If boundaries haven't changed, the function returns this result directly,
#'   avoiding recomputation. Default is NULL.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item \code{bdist3}: A \code{SpatRaster} with distances (km) from cell centroids
#'       to the border of the country.
#'     \item \code{boundaries}: An \code{sf} object with country boundary line geometries
#'   }
#'
#' @examples
#' \dontrun{
#' # Calculate border distances for 2010
#' border_dist_2010 <- bdist3(as.Date("2010-01-01"))
#' }
#' @export
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
bdist3 <- function(measurement_date, cshp = read_cshapes(), past_result = NULL){
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

  gwcodes <- cshapes_gwcode(measurement_date)
  sf::sf_use_s2(TRUE)
  bdist3 <- list()
  for(i in 1:nrow(boundaries)){
    gwcode <- boundaries[i,]$gwcode
    gwrast <- ifel(gwcodes == gwcode, 1, NA)

    boundary_vect <- boundaries[i,] |> sf::st_combine() |> terra::vect()

    # Get extent and add a buffer (e.g., 5 degrees on each side)
    country_ext <- terra::ext(boundary_vect)
    buffered_ext <- terra::extend(country_ext, 5)  # 5 degrees buffer

    # Crop to country extent first - this is the key speedup
    country_rast <- terra::crop(gwcodes, buffered_ext)
    country_rast <- ifel(country_rast == gwcode, 1, NA)

    # Now distance() only computes for cells in the cropped raster
    tmp <- terra::distance(country_rast, boundary_vect, rasterize = TRUE)
    bdist3[[as.character(gwcode)]] <- tmp * country_rast
  }
  bdist3 <- terra::mosaic(terra::sprc(bdist3))

  return(list("bdist3" = bdist3, "boundaries" = boundaries))
}


#' Generate distance to a country's own borders (bdist3)
#'
#' Creates a multi-layer raster containing the spherical distance (in kilometers) from each PRIO-GRID cell centroid
#' to the territorial outline of the country the cell belongs to using CShapes 2.0
#' boundary data for temporal time slices as defined in PRIOGRID.
#'
#' The function automatically uses past results from previous time slices to
#' reduce computation time when country boundaries remain unchanged
#' between consecutive periods.
#'
#' @param cshp An \code{sf} object containing CShapes 2.0 boundary data with
#'   temporal information. Defaults to \code{\link{read_cshapes}()} if not provided.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item This function is computationally intensive and may take hours to complete
#'   \item Progress indicators are printed during processing (time slice numbers)
#'   \item The optimization using past results significantly reduces total computation time
#'   \item Consider running in segments for very long time series to manage memory usage
#' }
#'
#' @examples
#' \dontrun{
#' # Generate full temporal border distance dataset
#' # Warning: This may take several hours to complete
#' temporal_bdist3 <- gen_bdist3()
#'
#' print(temporal_bdist3)
#' }
#' @export
#' @references
#' \insertRef{schvitzMappingInternationalSystem2022}{priogrid}
gen_bdist3 <- function(cshp = read_cshapes()){
  temporal_interval <- lubridate::interval(min(cshp$gwsdate), max(cshp$gwedate))
  time_slices <- pg_dates()
  time_slices <- time_slices[time_slices %within% temporal_interval]

  res <- bdist3(time_slices[1], cshp = cshp)
  r <- res$bdist3
  for(i in 2:length(time_slices)){
    print(i)
    t <- time_slices[i]
    res <- bdist3(t, cshp = cshp, past_result = res)
    terra::add(r) <- res$bdist3
  }
  names(r) <- as.character(time_slices)
  r
}
