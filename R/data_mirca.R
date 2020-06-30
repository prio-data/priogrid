#' @title grow_start
#'
#' @description Provides the starting month of the growing season in the cell, based on the Cropping Periods List data
#' from the MIRCA2000 dataset v.1.1. Snapshot for the year 2000 only.
#' Values range from 1-12. Defined as the first of five consecutive months with the highest harvested area.
#'
#' Link to original data: https://www.uni-frankfurt.de/45218031
#'
#' Please cite: Portmann, F. T., Siebert, S. & Döll, P. (2010). MIRCA2000 – Global monthly irrigated and rainfed crop areas around the year 2000: A new high-resolution data set for agricultural and hydrological modeling, Global Biogeochemical Cycles, 24, GB 1011, doi:10.1029/2008GB003435.
#'
#' @param input_folder path to [pg-folder].
#' @param no_months width of rolling window (number of months) used to compute max sum of harvested area. Defaults to 5.
#' @param variable one of c("grow_start", "grow_end")
#'
#' @return a dataframe with x, y, pgid, and grow_start month.
#'
#' @export

gen_growstart <- function(input_folder, variable = "grow_start", no_months = 5){
  mirca <- prep_growseas(input_folder)

  mirca <- mirca %>%
    dplyr::mutate(jan2 = jan, feb2 = feb, mar2 = mar, apr2 = apr, may2 = may) %>%
    tidyr::pivot_longer(cols = c(-long, -lat), names_to = "month", values_to = "area") %>%
    dplyr::group_by(long, lat) %>%
    dplyr::mutate(nummonth = rep(1:17)) %>%
    dplyr::arrange(long, lat, nummonth) %>%
    dplyr::mutate(id = dplyr::group_indices()) %>%
    dplyr::ungroup()

  data.table::setDT(mirca)
  mirca[,consec_mo := zoo::rollapply(area, width = no_months, sum, align = "right", fill = NA, partial = F), by = id]

  mirca <- mirca %>%
    dplyr::mutate(grow_start = ifelse(!is.na(consec_mo), nummonth - (no_months - 1), NA),
                  grow_end = ifelse(!is.na(consec_mo), nummonth, NA)) %>%
    dplyr::mutate(grow_start = ifelse(grow_start > 12, grow_start-12, grow_start),
                  grow_end = ifelse(grow_end > 12, grow_end-12, grow_end)) %>%
    dplyr::group_by(id) %>%
    dplyr::slice(which.max(consec_mo)) ### Does NOT consider ties - it chooses the FIRST max for each point by default


  grow_start <- raster::rasterFromXYZ(mirca[c("long", "lat", variable)], crs = priogrid::prio_crs(), digits = 5)
  grow_start <- priogrid::raster_to_pg(grow_start)
  grow_start <- priogrid::raster_to_tibble(gs, add_pg_index = TRUE)
  return(grow_start)
}


#' grow_end
#'
#' Provides the final month of the growing season in the cell, based on the Cropping Periods List data
#' from the MIRCA2000 dataset v.1.1. Snapshot for the year 2000 only.
#' Values range from 1-12. Defined as the last of five consecutive months with the highest harvested area.
#'
#' Link to original data: https://www.uni-frankfurt.de/45218031
#'
#' Please cite: Portmann, F. T., Siebert, S. & Döll, P. (2010). MIRCA2000 – Global monthly irrigated and rainfed crop areas around the year 2000: A new high-resolution data set for agricultural and hydrological modeling, Global Biogeochemical Cycles, 24, GB 1011, doi:10.1029/2008GB003435.
#'
#' @param input_folder path to [pg-folder].
#' @param no_months width of rolling window (number of months) used to compute max sum of harvested area. Defaults to 5.
#' @param variable one of c("grow_start", "grow_end")
#'
#' @return a dataframe with x, y, pgid, and grow_end month.
#'
#' @export

gen_growend <- function(input_folder, no_months = 5){
  gen_grow_start(input_folder, variable = "grow_end")
}


#' grow_area
#'
#' Monthly variable, providing the area proportion of each cell
#' that is being harvested each month. Based on the Cropping Periods List data
#' from the MIRCA2000 dataset v.1.1. Shapshot for the year 2000 only.
#'
#' Link to original data: https://www.uni-frankfurt.de/45218031
#'
#' Please cite: Portmann, F. T., Siebert, S. & Döll, P. (2010). MIRCA2000 – Global monthly irrigated and rainfed crop areas around the year 2000: A new high-resolution data set for agricultural and hydrological modeling, Global Biogeochemical Cycles, 24, GB 1011, doi:10.1029/2008GB003435.
#'
#' @param input_folder path to [pg-folder].
#' @param month_no if `TRUE`, months are reported as numeric, 1-12.
#'
#' @return a dataframe with x, y, pgid, month, and grow_area proportion.
#'
#' @export
gen_growarea <- function(input_folder, month_no = FALSE){
  mirca <- prep_growseas(input_folder)

  mirca <- suppressWarnings(raster::rasterFromXYZ(mirca[c("long", "lat", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                                                          "sep", "oct", "nov", "dec")], crs = priogrid::prio_crs(), digits = 5))

  mirca <- priogrid::raster_to_pg(mirca, aggregation_function = "mean")
  mirca <- priogrid::raster_to_tibble(mirca, add_pg_index = TRUE)

  mirca <- mirca %>%
    tidyr::pivot_longer(cols = c(-x, -y, -pgid),
                        values_to = "grow_area",
                        names_to = "month")

  if (month_no){
    mirca <- mirca %>%
      dplyr::mutate(month = match(tolower(month), tolower(month.abb)))
  }

  return(mirca)
}


# data preparation
prep_growseas <- function(input_folder){
  mirca <- read.table(gzfile(file.path(input_folder, "growing_periods_listed", "data", "CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT.gz")),
                      header = T, sep = "\t")

  m_small <- mirca %>%
    dplyr::group_by(lat, lon, start, end) %>%
    dplyr::summarize(area = sum(area))

  select_months <- function(start, end, drop_perennials=FALSE){
    if(is.na(start) | is.na(end)){
      return(rep(NA, 12))
    } else if(drop_perennials & start==1 & end==12){ # perennials have growing seasons 1:12
      return(rep(NA, 12))
    } else {
      if(start <= end){
        return(start:end)
      } else {
        return(c(start:12, 1:end))
      }
    }
  }

  data.table::setDT(m_small)

  m_small <- m_small[, list(lat = lat, lon = lon, area = area, start = start, end = end, month = priogrid::select_months(start, end)),
                     list(1:nrow(m_small))][, nrow := NULL][]

  m_small <- m_small[, list(area = sum(area, na.rm = T)), by = c("lat", "lon", "month")]


  m_small <- m_small %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::mutate(season_weight = area / sum(area)) %>%
    dplyr::select(lat, long = lon, month, season_weight) %>%
    tidyr::pivot_wider(names_from = "month",
                       values_from = "season_weight") %>%
    magrittr::set_names( c("lat", "long", tolower(month.abb)) )

  return(m_small)
}
