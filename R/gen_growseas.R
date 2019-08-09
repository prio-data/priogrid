
gen_growseas <- function(mirca_data){
  m_small <- prep_growseas(mirca_data)

  m_raster <- raster::rasterFromXYZ(m_small[c("long", "lat", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                                              "sep", "oct", "nov", "dec")], crs = priogrid::prio_crs(), digits = 1)

  agg <- priogrid::prio_aggregate_raster(m_raster, fun = mean)
  raster::extent(agg) <- priogrid::prio_extent()

  names(agg) <- tolower(month.abb)

  agg
}


# test <- gen_growseas("C:/Users/villar/Dropbox/pg_v3-dev/priogrid-dev/raw_data/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS.TXT.gz")



prep_growseas <- function(mirca_data){
  mirca <- read.table(gzfile(mirca_data), header = T, sep = "\t")

  m_small <- mirca %>%
    dplyr::group_by(lat, long, start, end) %>%
    dplyr::summarize(area = sum(area))

  data.table::setDT(m_small)
  m_small <- m_small[, .(lat = lat, long = long, area = area, start = start, end = end, month = select_months(start, end)),
                     .(1:nrow(m_small))][, nrow := NULL][]

  m_small <- m_small[, .(area = sum(area, na.rm = T)), by = c("lat", "long", "month")]


  m_small <- m_small %>%
    dplyr::group_by(lat, long) %>%
    dplyr::mutate(season_weight = area / sum(area)) %>%
    dplyr::select(lat, long, month, season_weight) %>%
    tidyr::spread(month, season_weight) %>%
    magrittr::set_names( c("lat", "long", tolower(month.abb)) )

  m_small
}


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





### HER:

m_small <- prep_growseas("C:/Users/villar/Dropbox/pg_v3-dev/priogrid-dev/raw_data/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS.TXT.gz")


head(m_small)




?zoo::rollapply
?zoo::zoo



m_small$jan.2 <- m_small$jan
m_small$feb.2 <- m_small$feb




tst <- m_small


tst <- zoo::rollsum(tst, 3, align = "right")


length(m_small$jan)
rollfun <- function(x, na.rm = na.rm) {zoo::rollsum(x, 3, align = "right")}
tst
?zoo::rollsum
