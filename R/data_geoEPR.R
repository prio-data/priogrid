#' Reads the GeoEPR - Geo-referencing Ethnic Power Relations data
#'
#' Downloads and processes GeoEPR (Geo-referencing Ethnic Power Relations) data,
#' which provides spatial information on politically relevant ethnic groups and
#' their settlement areas worldwide. The function formats temporal variables and
#' adds utility columns for temporal analysis compatible with PRIO-GRID.
#'
#' @details
#' GeoEPR combines the Ethnic Power Relations (EPR) dataset with geographic
#' information to map politically relevant ethnic groups' settlement patterns.
#' This function:
#' \itemize{
#'   \item Downloads the GeoEPR dataset from the ETH ICR data repository
#'   \item Converts temporal variables (from, to) to proper Date objects (gwsdate, gwedate)
#'   \item Adds a date_interval utility column for temporal operations
#'   \item Filters out geometries with empty spatial information
#'   \item Returns data in sf format for spatial analysis
#' }
#'
#' @seealso
#' \code{\link{pg_dates}} for PRIO-GRID temporal coverage,
#' \code{\link[sf]{st_sf}} for sf object details
#'
#' @examples
#' \dontrun{
#' # Read GeoEPR data
#' geoepr_data <- read_geoepr()
#'
#' print(geoepr_data)
#'
#' # Check available columns
#' names(geoepr_data)
#'
#' # View temporal coverage
#' summary(geoepr_data$gwsdate)
#' summary(geoepr_data$gwedate)
#'
#' # Filter to specific time period
#' modern_groups <- geoepr_data[geoepr_data$gwsdate >= as.Date("2000-01-01"), ]
#'
#' # Examine unique ethnic groups
#' unique_groups <- unique(geoepr_data$group)
#' head(unique_groups, 10)
#'
#' # Filter to specific country (example: using country code)
#' # country_groups <- geoepr_data[geoepr_data$gwid == 2, ]  # Example for USA
#'
#' # Plot ethnic group settlement areas
#' plot(sf::st_geometry(geoepr_data), main = "Ethnic Group Settlement Areas")
#' }
#' @return A \code{sf} object
#' @export
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{priogrid}
#' \insertRef{vogtIntegratingDataEthnicity2015}{priogrid}
read_geoepr <- function() {
  f <- get_pgfile(source_name = "ETH ICR GeoEPR",
                  source_version = "2023",
                  id = "3900b527-a728-4c26-b0ab-f4441d3ee2e8")

  df <- sf::st_read(f)

  df <- df |>
    dplyr::mutate(gwsdate = as.Date(paste0(from, "-01-01")),
           gwedate = as.Date(paste0(to, "-12-31"))) |>
    dplyr::mutate(date_interval = lubridate::interval(gwsdate, gwedate)) |>
    dplyr::filter(!sf::st_is_empty(geometry))

  return(df)
}

#' Reads the EPR Core 2023 data
#'
#' Downloads and processes EPR Core (Ethnic Power Relations) data, which provides
#' information on politically relevant ethnic groups and their access to state
#' power worldwide. The function formats temporal variables and adds utility
#' columns for temporal analysis compatible with PRIO-GRID.
#'
#' @details
#' EPR Core tracks the political status of ethnic groups that are politically
#' relevant in each country-year. This function:
#' \itemize{
#'   \item Downloads the EPR Core dataset from the ETH ICR data repository
#'   \item Converts temporal variables (from, to) to proper Date objects (gwsdate, gwedate)
#'   \item Adds a date_interval utility column for temporal operations
#'   \item Returns data in tibble format for analysis
#' }
#'
#' The dataset categorizes ethnic groups based on their access to executive power,
#' ranging from monopoly and dominance to discrimination and exclusion. Groups
#' are considered politically relevant if they are either represented in government,
#' engage in political competition, or face systematic discrimination.
#'
#' Political status categories typically include:
#' \itemize{
#'   \item Monopoly: Group holds exclusive power
#'   \item Dominance: Group holds dominant power position
#'   \item Senior/Junior Partner: Group shares power in coalition
#'   \item Powerless: Group lacks political influence
#'   \item Discriminated: Group faces systematic discrimination
#'   \item Self-exclusion: Group voluntarily excludes itself from politics
#' }
#'
#' @seealso
#' \code{\link{read_geoepr}} for spatial ethnic group data,
#' \code{\link{pg_dates}} for PRIO-GRID temporal coverage
#'
#' @examples
#' \dontrun{
#' # Read EPR Core data
#' epr_data <- read_epr()
#'
#' # Examine the structure
#' print(epr_data)
#' str(epr_data)
#'
#' # Check available columns
#' names(epr_data)
#'
#' # View temporal coverage
#' range(epr_data$gwsdate, na.rm = TRUE)
#' range(epr_data$gwedate, na.rm = TRUE)
#'
#' # Examine unique countries
#' unique_countries <- unique(epr_data$country)
#' head(sort(unique_countries), 10)
#'
#' # Filter to specific country
#' usa_groups <- epr_data[epr_data$gwid == 2, ]  # USA
#' print(usa_groups)
#'
#' # Analyze power status distribution
#' if("status" %in% names(epr_data)) {
#'   table(epr_data$status)
#' }
#'
#' # Filter groups active in specific year
#' target_date <- as.Date("2010-01-01")
#' active_2010 <- epr_data[target_date %within% epr_data$date_interval, ]
#' nrow(active_2010)
#'
#' # Analyze temporal changes for specific group
#' # example_group <- epr_data[epr_data$group == "African Americans", ]
#' # View(example_group)
#' }
#'
#' @return A \code{tibble} object
#' @export
#'
#' @references
#' \insertRef{vogtIntegratingDataEthnicity2015}{priogrid}
read_epr <- function() {
  f <- get_pgfile(source_name = "ETH ICR EPR Core",
                   source_version = "2023",
                   id = "287bfdf7-2f4f-402a-88df-5fe1f8b7046b")

  df <- readr::read_csv(f)

  df <- df |>
    dplyr::mutate(gwsdate = as.Date(paste0(from, "-01-01")),
                  gwedate = as.Date(paste0(to, "-12-31"))) |>
    dplyr::mutate(date_interval = lubridate::interval(gwsdate, gwedate))

  return(df)
}

#' Generate Multi-Temporal Regionally Excluded Ethnic Groups Coverage
#'
#' Creates a multi-layer raster indicating PRIO-GRID cells that contain settlement
#' areas of regionally based ethnic groups experiencing political exclusion. The
#' function combines GeoEPR spatial data with EPR Core political status information
#' to map areas where excluded ethnic groups are present over time.
#'
#' The function focuses on "regionally based" ethnic groups, which have identifiable
#' settlement patterns that can be meaningfully mapped, as opposed to groups that
#' are dispersed throughout a country. Political exclusion is defined by EPR
#' status codes indicating systematic discrimination, powerlessness, or voluntary
#' self-exclusion from political processes.
#'
#' @param excluded A character vector specifying EPR status codes to classify as
#'   "excluded". Valid EPR status codes include:
#'   \itemize{
#'     \item "DISCRIMINATED": Groups facing systematic discrimination
#'     \item "POWERLESS": Groups lacking political influence
#'     \item "SELF-EXCLUSION": Groups voluntarily excluding themselves from politics
#'     \item "MONOPOLY": Groups holding exclusive power
#'     \item "DOMINANT": Groups in dominant power positions
#'     \item "SENIOR PARTNER": Senior coalition partners
#'     \item "JUNIOR PARTNER": Junior coalition partners
#'     \item "IRRELEVANT": Groups without political relevance
#'     \item "STATE COLLAPSE": Groups during state collapse periods
#'   }
#'   Default is \code{c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION")}.
#'
#' @examples
#' \dontrun{
#' # Generate excluded groups coverage with default exclusion categories
#' excluded_coverage <- gen_geoepr_reg_excluded()
#'
#' # Examine the structure
#' print(excluded_coverage)
#'
#' # View available time periods
#' time_periods <- names(excluded_coverage)
#' head(time_periods)
#'
#' # Plot coverage for specific period
#' terra::plot(excluded_coverage[[1]],
#'             main = "Excluded Ethnic Groups Coverage")
#'
#' # Use custom exclusion definition (include only discriminated groups)
#' discriminated_only <- gen_geoepr_reg_excluded(excluded = "DISCRIMINATED")
#'
#' # Compare different exclusion definitions
#' powerless_only <- gen_geoepr_reg_excluded(excluded = "POWERLESS")
#' combined_exclusion <- gen_geoepr_reg_excluded(excluded = c("DISCRIMINATED", "POWERLESS"))
#'
#' # Analyze temporal changes in exclusion
#' first_period <- excluded_coverage[[1]]
#' last_period <- excluded_coverage[[nlyr(excluded_coverage)]]
#' exclusion_change <- last_period - first_period
#' terra::plot(exclusion_change, main = "Change in Excluded Group Presence")
#'
#' # Calculate total excluded area over time
#' total_excluded_cells <- terra::global(excluded_coverage > 0, "sum", na.rm = TRUE)
#' plot(total_excluded_cells$sum, type = "l",
#'      main = "Number of Cells with Excluded Groups Over Time")
#' }
#'
#' @return A \code{SpatRaster} object
#' @export
#'
#' @references
#' \insertRef{wucherpfennigPoliticallyRelevantEthnic2011}{priogrid}
#' \insertRef{vogtIntegratingDataEthnicity2015}{priogrid}
gen_geoepr_reg_excluded <- function(excluded = c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION")) {

  geoepr <- read_geoepr() |> dplyr::filter(type == "Regionally based") |>
    dplyr::select(from, to, gwgroupid, geometry)
  epr <- read_epr() |> dplyr::filter(status %in% excluded)
  pg <- prio_blank_grid()

  df <- dplyr::inner_join(geoepr, epr, by = c("gwgroupid", "from", "to"))

  df <- sf::st_transform(df, crs = sf::st_crs(pg))
  pg_intervals <- pg_date_intervals()
  pg_intervals <- pg_intervals[lubridate::int_start(pg_intervals) >= min(lubridate::int_start(df$date_interval))]
  pg_intervals <- pg_intervals[lubridate::int_end(pg_intervals) <= max(lubridate::int_end(df$date_interval))]

  make_raster <- function(current_interval, pg, df){
    sdf <- df |> dplyr::filter(lubridate::int_overlaps(date_interval, current_interval))

    if(nrow(sdf) > 0){
      sdf <- sdf |> dplyr::summarize(geometry = sf::st_combine(geometry))
      pg <- prio_blank_grid()
      coversh <- exactextractr::exact_extract(pg, sdf)
      ra <- exactextractr::rasterize_polygons(sdf, pg)
      pg <- pg*ra # Remove non-land cells
      res <- terra::classify(pg, coversh[[1]])
      return(res)
    }
  }

  r <- make_raster(pg_intervals[1], pg, df)
  for(i in 2:length(pg_intervals)){
    terra::add(r) <- make_raster(pg_intervals[i], pg, df)
  }
  names(r) <- as.character(lubridate::int_end(pg_intervals))
  return(r)
}

