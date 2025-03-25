#' Reads the AidData World Bank Geocoded Research Release, Version 1.4.2 project and locations data
#' It merges projects and locations into one dataframe
#' Removes missing values on location and project end date
#' Adds date interval column
#'
#'@param unknown_enddate default NA
#'
#' @return an object of class sf
#' @export
#'
#' @references{isakssonAidInstitutionsLocal2023}
read_aiddata <- function(unknown_enddate = NA) {
  zip_files <- get_pgfile(source_name = "World Bank Geocoded Research Release",
                  source_version = "1.4.2",
                  id = "52ac3e7e-b509-4d85-83b7-1875cb2b3afa")

  files_to_unzip <- unzip(zip_files[1], list = T) |> dplyr::filter(grepl("locations.csv|projects.csv", Name)) |>
    dplyr::pull(Name)

  unzip(zip_files[1], files = files_to_unzip, exdir = dirname(zip_files[1]))

  abs_path <- file.path(dirname(zip_files[1]), files_to_unzip)
  locations <- readr::read_csv(abs_path[1])
  projects <- readr::read_csv(abs_path[2])


  m <- dplyr::left_join(projects, locations, by = "project_id")
  missing_project_locations <- m |> dplyr::filter(is.na(longitude)) |> nrow()
  paste("There are", missing_project_locations, "projects without location data. These are dropped.")
  m <- m |> dplyr::filter(!is.na(longitude))
  m <- sf::st_as_sf(m, coords = c("longitude", "latitude"), crs = "epsg:4326")

  missing_project_enddate <- m |> dplyr::filter(is.na(end_actual_isodate)) |> nrow()
  paste("There are", missing_project_enddate, "projects without end date. These are dropped.")

  m <- m |>
    dplyr::filter(!is.na(end_actual_isodate)) |>
    dplyr::mutate(date_interval = lubridate::interval(start_actual_isodate, end_actual_isodate))


   return(m)
}

#' Generates project centroids for AidData projects
#'
#' We do not know the extent of each project, only the centroid as point coordinate
#' Each aid project location with its given time interval is matched with the PRIO-GRID cell it overlaps with
#'
#'
#' @return SpatRaster
#' @export
#'
#' @examples
#' # r <- gen_aiddata_project_centroids()
#'
#'
#' @references{isakssonAidInstitutionsLocal2023}
gen_aiddata_project_centroids <- function() {

  f <- read_aiddata()
  pg <- prio_blank_grid()

  time_intervals <- pg_date_intervals()

  stack_list <- list()
  for (i in 1:length(time_intervals)) {
    time_interval <- time_intervals[i]
    c <- f |> dplyr::filter(lubridate::int_overlaps(date_interval, time_interval))
    if(nrow(c) > 0){
      r <- terra::rasterize(c, pg, fun = "sum", na.rm = TRUE)
      end_t <- lubridate::int_end(time_interval)
      stack_list[[as.character(end_t)]] <- r
    }

  }

  stack <- terra::rast(stack_list)
  names(stack) <- names(stack_list)
  return(stack)
}



