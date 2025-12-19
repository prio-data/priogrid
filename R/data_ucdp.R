#' Reads the UCDP Georeferenced Event Dataset (GED)
#'
#' Downloads and processes the Uppsala Conflict Data Program's Georeferenced
#' Event Dataset (UCDP GED), which provides detailed information on individual
#' events of organized violence worldwide. The function formats the data as a
#' spatial object compatible with PRIO-GRID temporal analysis.
#'
#' @details
#' UCDP GED is the most disaggregated dataset on organized violence, containing
#' information on individual events including their location, timing, actors,
#' and fatality estimates. This function:
#' \itemize{
#'   \item Downloads UCDP GED version 25.1 if not already cached
#'   \item Converts the data to an sf spatial object with WGS84 coordinates
#'   \item Adds a date_interval utility column for temporal operations
#'   \item Preserves all original UCDP GED variables including fatality estimates
#' }
#'
#' The dataset includes three types of organized violence:
#' \itemize{
#'   \item Type 1: State-based armed conflict
#'   \item Type 2: Non-state conflict
#'   \item Type 3: One-sided violence
#' }
#'
#' @return A \code{sf} object containing georeferenced conflict events with:
#'   \itemize{
#'     \item Geometry: Point locations of conflict events
#'     \item date_start, date_end: Event timing information
#'     \item date_interval: Lubridate interval for temporal operations
#'     \item best, low, high: Fatality estimates (best estimate, low bound, high bound)
#'     \item type_of_violence: Violence type classification (1, 2, or 3)
#'     \item where_prec: Spatial precision code (1-7)
#'     \item Coordinate reference system: WGS84 (EPSG:4326)
#'   }
#'
#' @seealso
#' \code{\link{ucdp_ged}} for rasterizing events to PRIO-GRID,
#' \code{\link{ged_ucdp_ged}} for generating the standard UCDP GED variable
#'
#' @examples
#' \dontrun
#' # Read UCDP GED data
#' ged_data <- read_ucdp_ged()
#'
#' # Examine the structure
#' print(ged_data)
#'
#' # Check available columns
#' names(ged_data)
#'
#' # Filter to state-based conflict only
#' state_conflict <- ged_data[ged_data$type_of_violence == 1, ]
#'
#' # Plot event locations
#' plot(sf::st_geometry(ged_data), pch = ".",
#'      main = "UCDP GED Event Locations")
#'
#' # Filter to specific time period
#' library(lubridate)
#' events_2020 <- ged_data[year(ged_data$date_start) == 2020, ]
#' }
#'
#' @export
#' @references
#' \insertRef{sundbergIntroducingUCDPGeoreferenced2013}{priogrid}
#'
#' \insertRef{daviesOrganizedViolence198920242025}{priogrid}
read_ucdp_ged <- function(){
  f <- get_pgfile(source_name = "UCDP GED",
                  source_version = "25.1",
                  id = "49f79d96-4e4d-4812-9dd1-862bacfca577")

  unzip(f, exdir = dirname(f))
  df <- readRDS(file.path(dirname(f), unzip(f, list = TRUE)$Name)) |>
    sf::st_as_sf(crs = 4326, coords = c("longitude", "latitude")) |>
    dplyr::mutate(date_interval = lubridate::interval(date_start, date_end))
  #plot(sf::st_geometry(df), pch = ".")

  return(df)
}


#' Rasterize UCDP GED Events for a Single Time Interval
#'
#' Internal function that aggregates UCDP GED conflict events to the PRIO-GRID
#' raster structure for a specified time interval. The function filters events
#' by temporal overlap and spatial precision before rasterization.
#'
#' @details
#' This function performs the following operations:
#' \itemize{
#'   \item Transforms event coordinates to match PRIO-GRID projection
#'   \item Filters events to those overlapping the specified time interval
#'   \item Removes events with spatial precision code >= 6 (sub-national precision required)
#'   \item Filters by specified violence types
#'   \item Rasterizes events by summing the specified fatality variable or counting events
#' }
#'
#' Spatial precision filtering removes events coded at national level or higher
#' uncertainty, which may lead to undercounting in some regions.
#'
#' @param ged An \code{sf} object containing UCDP GED data, typically from
#'   \code{\link{read_ucdp_ged}} or processed through \code{\link{ucdp_ged}}.
#' @param pg_interval A \code{lubridate} interval object specifying the time
#'   period for aggregation.
#' @param fatality_variable Character string specifying which fatality estimate
#'   to aggregate. Options include "best", "low", "high", or "event_count" for
#'   counting events rather than fatalities.
#'
#' @return A \code{SpatRaster} object with aggregated values for the specified
#'   time interval. Layer name is set to the interval end date.
#'
#' @seealso
#' \code{\link{ucdp_ged}} for the main processing function,
#' \code{\link{prio_blank_grid}} for PRIO-GRID structure
#'
#' @keywords internal
#' @references
#' \insertRef{sundbergIntroducingUCDPGeoreferenced2013}{priogrid}
#'
#' \insertRef{daviesOrganizedViolence198920242025}{priogrid}
rasterize_ged_crossection <- function(ged, pg_interval, fatality_variable){
  pg <- prio_blank_grid()
  ged <- ged |>
    sf::st_transform(crs = sf::st_crs(pg)) |>
    dplyr::filter(lubridate::int_overlaps(date_interval, pg_interval)) |>
    dplyr::filter(where_prec < 6) |> # Remove where_prec >= 6 (i.e. not sub-national precision)
    dplyr::filter(type_of_violence %in% violence_types)
  if(fatality_variable == "event_count"){
    r <- terra::rasterize(ged, pg,  fun = "sum") # count events
  } else{
    r <- terra::rasterize(ged, pg, field = fatality_variable,  fun = "sum") # count e.g., battle-related deaths
  }
  names(r) <- as.character(lubridate::int_end(pg_interval))
  r
}

#' Process UCDP GED Data with Temporal Fatality Distribution
#'
#' Processes UCDP GED conflict data by distributing fatalities across PRIO-GRID
#' temporal intervals and rasterizing the results. Events spanning multiple
#' time periods have their fatalities proportionally allocated based on temporal
#' overlap, ensuring accurate representation in panel data structures.
#'
#' @details
#' This function implements a sophisticated fatality distribution algorithm that:
#' \itemize{
#'   \item Calculates the temporal overlap between each event and PRIO-GRID intervals
#'   \item Distributes fatalities proportionally based on the number of overlapping days
#'   \item Uses integer allocation with remainder distribution to preserve total counts
#'   \item Handles edge cases where events span multiple periods or have single fatalities
#'   \item Validates that distributed fatalities sum to original event totals
#' }
#'
#' The distribution algorithm ensures that:
#' \itemize{
#'   \item Total fatalities are preserved (no over- or under-counting)
#'   \item Single-fatality events are assigned to exactly one period
#'   \item Fractional allocations are resolved using largest remainder method
#' }
#'
#' Events without sub-national spatial precision (where_prec >= 6) are excluded
#' from the final rasterization, which may result in undercounting in some regions.
#'
#' @param ged An \code{sf} object containing UCDP GED data. Defaults to
#'   \code{\link{read_ucdp_ged}()} if not provided.
#' @param violence_types Integer vector specifying which types of violence to include.
#'   Options are:
#'   \itemize{
#'     \item 1: State-based armed conflict
#'     \item 2: Non-state conflict
#'     \item 3: One-sided violence
#'   }
#'   Default is \code{c(1, 2, 3)} (all types).
#' @param fatality_variable Character string specifying which fatality estimate
#'   to use. Options are:
#'   \itemize{
#'     \item "best": Best estimate of fatalities (default)
#'     \item "low": Low estimate of fatalities
#'     \item "high": High estimate of fatalities
#'     \item "event_count": Count of events rather than fatalities
#'   }
#'
#' @return A \code{SpatRaster} object with multiple layers, one for each time
#'   interval in \code{\link{pg_date_intervals}}. Layer names correspond to
#'   interval end dates. Cell values represent aggregated fatalities (or event
#'   counts) for each grid cell and time period.
#'
#' @note
#' \itemize
#'   \item The function requires the data.table package for efficient processing
#'   \item Processing time scales with the number of events and time intervals
#'   \item A warning is issued if fatality distribution validation fails
#'   \item Events coded at national level or coarser precision are excluded
#' }
#'
#' @seealso
#' \code{\link{read_ucdp_ged}} for loading raw UCDP GED data,
#' \code{\link{ged_ucdp_ged}} for the standard generator function,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal structure
#'
#' @examples
#' \dontrun{
#' # Process all violence types with best fatality estimates
#' ged_raster <- ucdp_ged()
#'
#' # Examine the result
#' print(ged_raster)
#'
#' # Process state-based conflict only
#' state_conflict <- ucdp_ged(violence_types = 1)
#'
#' # Use high fatality estimates
#' ged_high <- ucdp_ged(fatality_variable = "high")
#'
#' # Count events instead of fatalities
#' event_counts <- ucdp_ged(fatality_variable = "event_count")
#'
#' # Plot a single time slice
#' terra::plot(ged_raster[[1]],
#'             main = paste("UCDP GED Fatalities:", names(ged_raster)[1]))
#'
#' # Calculate total fatalities across all periods
#' total_fatalities <- terra::app(ged_raster, sum, na.rm = TRUE)
#' terra::plot(total_fatalities, main = "Total Fatalities (All Periods)")
#' }
#'
#' @export
#' @references
#' \insertRef{sundbergIntroducingUCDPGeoreferenced2013}{priogrid}
#'
#' \insertRef{daviesOrganizedViolence198920242025}{priogrid}
ucdp_ged <- function(ged = read_ucdp_ged(), violence_types = c(1,2,3), fatality_variable = "best"){
  require(data.table)
  distribute_fatalities <- function(ged_dt, value_var = "best") {
    if(!"event_id" %in% names(ged_dt)) {
      ged_dt[, event_id := .I]
    }

    agg_intervals <- pg_date_intervals()
    agg_dt <- data.table(
      interval_id = seq_along(agg_intervals),
      agg_start = int_start(agg_intervals),
      agg_end = int_end(agg_intervals)
    )

    # >>> FIX: Preserve original event dates before the join <
    ged_dt[, `:=`(
      event_start = date_start,
      event_end = date_end
    )]

    ged_dt[, event_days := as.numeric(event_end - event_start) + 1]

    # Non-equi join (date_start/date_end will be overwritten)
    result <- ged_dt[agg_dt,
                     on = .(date_start <= agg_end, date_end >= agg_start),
                     allow.cartesian = TRUE,
                     nomatch = NULL
    ]

    result <- merge(result, agg_dt, by = "interval_id")

    # >>> FIX: Use preserved event_start/event_end <
    result[, `:=`(
      overlap_start = pmax(event_start, agg_start),
      overlap_end = pmin(event_end, agg_end)
    )]

    result[, overlap_days := as.numeric(overlap_end - overlap_start) + 1]

    result[, `:=`(
      prop_share = overlap_days / event_days,
      total_fatalities = get(value_var)
    )]

    result[, base_count := floor(total_fatalities * prop_share)]
    result[, residual := total_fatalities[1] - sum(base_count), by = event_id]
    result[, frac_part := (total_fatalities * prop_share) - base_count]
    result[, rank := frank(-frac_part, ties.method = "random"), by = event_id]
    result[, distributed_count := base_count + as.integer(rank <= residual)]

    result[, is_last_period := rank == max(rank), by = event_id]
    result[total_fatalities > 1 & sum(base_count) == 0,
           distributed_count := fifelse(is_last_period & distributed_count == 0, 1L, distributed_count),
           by = event_id]

    check <- result[, .(
      original = total_fatalities[1],
      distributed = sum(distributed_count)
    ), by = event_id]

    if(any(check$original != check$distributed, na.rm = TRUE)) {
      warning("Some events have mismatched fatality counts")
    }

    return(result)
  }

  ged <- ged |> dplyr::filter(type_of_violence %in% violence_types)
  ged_dt <- sf::st_drop_geometry(ged) |> dplyr::select(id, date_start, date_end, date_prec, dplyr::all_of(fatality_variable))
  setDT(ged_dt)
  result <- distribute_fatalities(ged_dt, value_var = fatality_variable)

  agg_result <- result[, .(
    fatalities = sum(distributed_count),
    agg_start = first(agg_start),
    agg_end = first(agg_end)
  ), by = .(interval_id, id)]


  idx <- match(agg_result$id, ged$id)
  agg_result[, `:=`(
    type_of_violence = ged$type_of_violence[idx],
    where_prec = ged$where_prec[idx]
  )]
  agg_result$geometry <- sf::st_geometry(ged)[idx]
  agg_sf <- sf::st_as_sf(agg_result, crs = sf::st_crs(ged))

  time_intervals <- pg_date_intervals()
  ged_interval <- lubridate::interval(min(ged$date_start), max(ged$date_end))
  time_intervals <- time_intervals[time_intervals %within% ged_interval]

  agg_sf <- agg_sf |> dplyr::rename(date_start= agg_start, date_end = agg_end, !!(fatality_variable) := fatalities)
  agg_sf <- agg_sf |> dplyr::mutate(date_interval = lubridate::interval(date_start, date_end))
  r <- rasterize_ged_crossection(agg_sf, pg_interval = time_intervals[1], fatality_variable = fatality_variable)
  for(i in 2:length(time_intervals)){
    t <- time_intervals[i]
    rt <- rasterize_ged_crossection(agg_sf, pg_interval = t, fatality_variable = fatality_variable)
    terra::add(r) <- rt
  }
  names(r) <- as.character(lubridate::int_end(time_intervals))
  r
}

#' Generate UCDP GED Battle-Related Deaths Variable
#'
#' Creates a multi-layer raster containing aggregated fatality counts from the
#' UCDP Georeferenced Event Dataset for all PRIO-GRID temporal slices. This is
#' the standard generator function for including UCDP GED data in PRIO-GRID.
#'
#' @details
#' This function generates the standard UCDP GED variable for PRIO-GRID using:
#' \itemize{
#'   \item The "best" fatality estimate from UCDP GED
#'   \item All three types of organized violence (state-based, non-state, one-sided)
#'   \item Temporal distribution of fatalities across PRIO-GRID intervals
#'   \item Spatial filtering to exclude events without sub-national precision
#' }
#'
#' Events are filtered to include only those with spatial precision codes 1-5,
#' which represent locations identified at sub-national level or better. Events
#' coded at national level (where_prec = 6) or with higher uncertainty (where_prec = 7)
#' are excluded. This filtering approach may result in undercounting, particularly
#' in regions where precise event locations are difficult to determine.
#'
#' The function uses the "best" fatality estimate, which represents UCDP's
#' assessment of the most likely number of fatalities based on available sources.
#' Alternative estimates (low, high) can be accessed through \code{\link{ucdp_ged}}.
#'
#' @return A \code{SpatRaster} object with multiple layers, one for each time
#'   interval defined in \code{\link{pg_date_intervals}}. Each layer contains:
#'   \itemize{
#'     \item Cell values: Sum of best fatality estimates for events in that cell and period

#'     \item Layer names: Character representation of interval end dates
#'     \item NA values: Grid cells with no recorded events or outside state system
#'   }
#'
#' @note
#' \itemize{
#'   \item Processing time depends on the size of the UCDP GED dataset
#'   \item The exclusion of imprecisely located events is a known limitation
#'   \item For custom configurations, use \code{\link{ucdp_ged}} directly
#' }
#'
#' @seealso
#' \code{\link{ucdp_ged}} for customizable processing options,
#' \code{\link{read_ucdp_ged}} for raw data access,
#' \code{\link{pg_date_intervals}} for temporal structure
#'
#' @examples
#' \dontrun{
#' # Generate standard UCDP GED variable
#' ged_fatalities <- ged_ucdp_ged()
#'
#' # Examine the result
#' print(ged_fatalities)
#'
#' # Check number of time periods
#' terra::nlyr(ged_fatalities)
#'
#' # Plot first time period
#' terra::plot(ged_fatalities[[1]],
#'             main = paste("Fatalities:", names(ged_fatalities)[1]))
#'
#' # Calculate annual totals
#' annual_total <- terra::app(ged_fatalities, sum, na.rm = TRUE)
#' terra::plot(annual_total, main = "Total Fatalities Across All Periods",
#'             col = hcl.colors(100, "YlOrRd", rev = TRUE))
#'
#' # Identify hotspots
#' hotspots <- annual_total > 100
#' terra::plot(hotspots, main = "Conflict Hotspots (>100 fatalities)")
#' }
#'
#' @export
#' @references
#' \insertRef{sundbergIntroducingUCDPGeoreferenced2013}{priogrid}
#'
#' \insertRef{daviesOrganizedViolence198920242025}{priogrid}
ged_ucdp_ged <- function(){
  ucdp_ged(ged = read_ucdp_ged(), violence_types = c(1, 2, 3), fatality_variable = "best")
}


ucdpged_distance_within_country <- function(measurement_date, ged = read_ucdp_ged(), cshp = read_cshapes()){
  cshp <- cshp |> dplyr::filter(measurement_date %within% date_interval)

  pg_interval <- pg_date_intervals()[measurement_date %within% pg_date_intervals()]

  pg <- prio_blank_grid()
  ged <- ged |>
    sf::st_transform(crs = sf::st_crs(pg)) |>
    dplyr::filter(lubridate::int_overlaps(date_interval, pg_interval)) |>
    dplyr::filter(where_prec < 6) # Remove where_prec >= 6 (i.e. not sub-national precision)

  rgw <- cshapes_gwcode(measurement_date, cshp = cshp)
  gwcodes <- unique(cshp$gwcode)

  cover <- cshapes_cover(measurement_date, cshp = cshp)
  values(cover) <- dplyr::if_else(values(cover) == T, 1, NA)

  result <- pg
  result[values(result)] <- 0
  result <- result*cover
  suppressMessages(sf::sf_use_s2(FALSE)) # Only use sf here to subset data.
  distances <- list()
  for(gwcode in gwcodes){
    tmp <- rgw
    tmp[values(tmp) != gwcode] <- NA
    tmp[!is.na(values(tmp))] <- 1

    suppressMessages(
      ged_sub <- ged[sf::st_intersects(ged, cshp[cshp$gwcode == gwcode, ], sparse = FALSE),]
    )

    if(nrow(ged_sub) > 0){
      dist <- terra::distance(tmp, terra::vect(ged_sub), rasterize = T)
      dist <- (dist*tmp)
      dist[is.na(dist)] <- 0
      result <- result + dist
    } else{
      # If no GED events in country, then set the distance to missing (to separate from being 0 m from events)
      tmp[is.na(values(tmp))] <- 0
      tmp[values(tmp) == 1] <- NA
      tmp[values(tmp) == 0] <- 1
      result <- result * tmp
    }
  }
  suppressMessages(sf::sf_use_s2(TRUE))

  #plot(1/log1p(result))
  #plot(1/result)
  return(result)
}
