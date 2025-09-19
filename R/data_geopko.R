#' Reads the Geocoded Peacekeeping Operations (Geo-PKO) data
#'
#' Downloads and processes Geo-PKO (Geocoded Peacekeeping Operations) data,
#' which provides spatial and temporal information on UN peacekeeping deployments
#' worldwide. The function formats coordinates as spatial geometries and
#' standardizes temporal variables for analysis.
#'
#' The dataset contains information on peacekeeping deployments including
#' mission details, troop numbers, deployment locations, and temporal coverage.
#' This enables analysis of peacekeeping presence, intensity, and geographic
#' distribution over time.
#'
#' @details
#' Geo-PKO tracks the locations and timing of UN peacekeeping operations at
#' high spatial and temporal resolution. This function:
#' \itemize{
#'   \item Downloads the Geo-PKO dataset from the data repository
#'   \item Filters out records with missing coordinate information
#'   \item Converts longitude/latitude coordinates to sf point geometries
#'   \item Sets coordinate reference system to WGS84 (EPSG:4326)
#'   \item Creates standardized date variables (mydate) from year-month information
#'   \item Rolls dates forward to the last day of each month for consistency
#' }
#'
#' @return A \code{sf} object
#'
#' @examples
#' \dontrun{
#' # Read Geo-PKO data
#' geopko_data <- read_geopko()
#'
#' # Examine the structure
#' print(geopko_data)
#' str(geopko_data)
#'
#' # Check available columns
#' names(geopko_data)
#'
#' # View temporal coverage
#' range(geopko_data$mydate, na.rm = TRUE)
#'
#' # Examine unique missions
#' if("mission" %in% names(geopko_data)) {
#'   unique_missions <- unique(geopko_data$mission)
#'   head(sort(unique_missions), 10)
#' }
#'
#' # Plot peacekeeping deployment locations
#' plot(sf::st_geometry(geopko_data),
#'      main = "UN Peacekeeping Deployment Locations",
#'      pch = 16, cex = 0.5)
#'
#' # Filter to specific time period
#' recent_deployments <- geopko_data[geopko_data$mydate >= as.Date("2010-01-01"), ]
#' plot(sf::st_geometry(recent_deployments),
#'      main = "Recent Peacekeeping Deployments (2010+)",
#'      pch = 16, cex = 0.5)
#'
#' # Analyze deployment intensity by year
#' geopko_data$year_deployed <- lubridate::year(geopko_data$mydate)
#' yearly_deployments <- table(geopko_data$year_deployed)
#' plot(names(yearly_deployments), yearly_deployments,
#'      type = "l", main = "Peacekeeping Deployments Over Time",
#'      xlab = "Year", ylab = "Number of Deployments")
#'
#' # Filter to specific mission (example)
#' # monusco_data <- geopko_data[grepl("MONUSCO", geopko_data$mission), ]
#' # plot(sf::st_geometry(monusco_data), main = "MONUSCO Deployments")
#' }
#'
#' @export
#'
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
read_geopko <- function(){
  f <- get_pgfile(source_name = "Geocoded Peacekeeping Operations (Geo-PKO)",
                  source_version = "2.2",
                  id = "7dcbfbfb-9667-4684-af34-85f69fa8d0a0")
  df <- readRDS(f)
  df <- df |>
    dplyr::filter(!is.na(longitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 remove = FALSE) |>
    sf::st_set_crs(4326)

  # Set date to last day in month
  df$mydate <- lubridate::ym(paste(df$year, df$month, sep = "-")) |> lubridate::rollforward()

  return(df)
}

#' Generate Multi-Temporal Peacekeeping Operations Count Data
#'
#' Creates a multi-layer raster containing counts of UN peacekeeping operations
#' within each PRIO-GRID cell across all temporal intervals. The function
#' aggregates Geo-PKO deployment locations to grid cells, providing a measure
#' of peacekeeping intensity and geographic distribution over time.
#'
#' @return A \code{SpatRaster} object
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate peacekeeping operations count data
#' pko_counts <- gen_geopko_operations_count()
#'
#' # Examine the structure
#' print(pko_counts)
#'
#' # View available time periods
#' time_periods <- names(pko_counts)
#' head(time_periods)
#' tail(time_periods)
#'
#' # Plot operations count for specific period
#' terra::plot(pko_counts[[1]],
#'             main = paste("Peacekeeping Operations Count:", time_periods[1]))
#'
#' # Find periods with highest peacekeeping activity
#' total_operations <- terra::global(pko_counts, "sum", na.rm = TRUE)
#' max_activity_period <- which.max(total_operations$sum)
#' terra::plot(pko_counts[[max_activity_period]],
#'             main = paste("Peak Activity Period:", time_periods[max_activity_period]))
#'
#' # Analyze temporal trends in peacekeeping deployment
#' yearly_totals <- terra::global(pko_counts, "sum", na.rm = TRUE)
#' plot(1:nlyr(pko_counts), yearly_totals$sum,
#'      type = "l", main = "Total Peacekeeping Operations Over Time",
#'      xlab = "Time Period", ylab = "Total Operations Count")
#'
#' # Identify hotspots of peacekeeping activity
#' total_pko_activity <- terra::app(pko_counts, sum, na.rm = TRUE)
#' terra::plot(total_pko_activity,
#'             main = "Cumulative Peacekeeping Operations")
#'
#' # Compare early vs. recent periods
#' early_period <- pko_counts[[1:5]]  # First 5 periods
#' recent_period <- pko_counts[[(nlyr(pko_counts)-4):nlyr(pko_counts)]]  # Last 5 periods
#' early_total <- terra::app(early_period, sum, na.rm = TRUE)
#' recent_total <- terra::app(recent_period, sum, na.rm = TRUE)
#' terra::plot(c(early_total, recent_total),
#'             main = c("Early Period PKO", "Recent Period PKO"))
#' }
#'
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_operations_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  time_intervals <- pg_date_intervals()
  stack_list <- list()
  for (i in 1:length(time_intervals)) {
    t <- time_intervals[i]
    c <- f |> dplyr::filter(mydate %within% !!t)
    if(nrow(c) > 0){
      r <- terra::rasterize(c, pg, fun = "sum", na.rm = TRUE)
      end_t <- lubridate::int_end(t)
      stack_list[[as.character(end_t)]] <- r
    }

  }

  stack <- terra::rast(stack_list)
  names(stack) <- names(stack_list)
  return(stack)

}

#' Generate Multi-Temporal Peacekeeping Troop Deployment Data
#'
#' Creates a multi-layer raster containing the total number of UN peacekeeping
#' troops deployed within each PRIO-GRID cell across all temporal intervals.
#' The function aggregates Geo-PKO troop deployment data to grid cells,
#' providing a measure of peacekeeping force strength and geographic distribution over time.
#'
#' @return A \code{SpatRaster} object
#'
#' @note
#' \itemize{
#'   \item Troop numbers marked as "unknown" in the source data are converted to NA and excluded
#'   \item Multiple deployments in the same cell are summed to give total troop presence
#'   \item Values represent troop numbers at deployment locations, not area-wide estimates
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate peacekeeping troop deployment data
#' pko_troops <- gen_geopko_troops_count()
#'
#' # Examine the structure
#' print(pko_troops)
#'
#' # View available time periods
#' time_periods <- names(pko_troops)
#' head(time_periods)
#'
#' # Plot troop deployment for specific period
#' terra::plot(pko_troops[[1]],
#'             main = paste("Peacekeeping Troops:", time_periods[1]))
#'
#' # Find periods with largest troop deployments
#' total_troops <- terra::global(pko_troops, "sum", na.rm = TRUE)
#' max_deployment_period <- which.max(total_troops$sum)
#' terra::plot(pko_troops[[max_deployment_period]],
#'             main = paste("Peak Deployment:", time_periods[max_deployment_period]))
#'
#' # Analyze temporal trends in troop deployment
#' yearly_troop_totals <- terra::global(pko_troops, "sum", na.rm = TRUE)
#' plot(1:nlyr(pko_troops), yearly_troop_totals$sum,
#'      type = "l", main = "Total Peacekeeping Troops Over Time",
#'      xlab = "Time Period", ylab = "Total Troops Deployed")
#'
#' # Identify major deployment locations
#' cumulative_troops <- terra::app(pko_troops, sum, na.rm = TRUE)
#' terra::plot(cumulative_troops,
#'             main = "Cumulative Peacekeeping Troop Deployments")
#'
#' # Compare troop numbers vs. operation counts
#' pko_operations <- gen_geopko_operations_count()
#' # Compare specific periods
#' period_idx <- 10  # Example period
#' terra::plot(c(pko_operations[[period_idx]], pko_troops[[period_idx]]),
#'             main = c("Operations Count", "Troop Numbers"))
#'
#' # Analyze troop concentration (troops per operation)
#' troops_per_op <- pko_troops / pko_operations
#' terra::plot(troops_per_op[[period_idx]],
#'             main = "Average Troops per Operation")
#' }
#'
#' @references
#' \insertRef{cilMappingBlueHelmets2020}{priogrid}
gen_geopko_troops_count <- function() {
  f <- read_geopko()
  pg <- prio_blank_grid()

  f$no.troops <- ifelse(f$no.troops == "unknown", NA, f$no.troops)
  f$no.troops <- as.numeric(f$no.troops)

  time_intervals <- pg_date_intervals()
  stack_list <- list()
  for (i in 1:length(time_intervals)) {
    t <- time_intervals[i]

    c <- f |> dplyr::filter(mydate %within% !!t)
    if(nrow(c) > 0){
      r <- terra::rasterize(c, pg, field = "no.troops", fun = "sum", na.rm = TRUE)
      end_t <- lubridate::int_end(t)
      stack_list[[as.character(end_t)]] <- r
    }
  }

  stack <- terra::rast(stack_list)
  names(stack) <- names(stack_list)
  return(stack)
}



