library(priogrid)
library(sf)
library(spex)
library(tidyverse)
library(lubridate)
library(parallel)

# Should this be a function?
pg <- create_pg_indices(prio_ncol(), prio_nrow())
pg <- raster::raster(pg)
crs(pg) <- prio_crs()
extent(pg) <- prio_extent()
pg_poly <- polygonize(pg)

tmp_dir <- tempdir()
tmp <- tempfile(tmpdir = tmp_dir)
download.file("http://downloads.weidmann.ws/cshapes/Shapefiles/cshapes_0.6.zip", tmp)
unzip(tmp, list = T)
unzip(tmp, exdir = tmp_dir)

fname <- paste(tmp_dir, "/cshapes.shp", sep = "")

cshp <- st_read(fname)
plot(st_geometry(cshp))

cshp <- cshp %>%
  filter(GWCODE != -1) %>%
  mutate(
    startdate = ymd(paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")),
    enddate = ymd(paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-"))
  ) %>%
  mutate(
    date_interval = interval(startdate, enddate)
  )

# The stuff below can be done for any monthly cross-section, but we should come
# up with a smarter algorithm so that we do not need to calculate everything
# anew every month.
#### I have just started here with some code that could be a smarter algorithm...
compare_crossection <- function(crossection_date, cshp){
  if(crossection_date - months(1) < min(cshp$startdate)){
    res <- tibble(date = crossection_date, change = 1)
  } else {
    past_crossection <- st_combine(cshp[(crossection_date - months(1)) %within% cshp$date_interval,])
    cshp_crossection <- st_combine(cshp[crossection_date %within% cshp$date_interval,])

    if( st_equals_exact(cshp_crossection, past_crossection, par = 0, sparse = F) ){
      res <- tibble(date = crossection_date, change = 0)
    } else{
      res <- tibble(date = crossection_date, change = 1)
    }

  }
  return(res)
}

all_months <- seq(min(cshp$startdate), max(cshp$enddate), by = "1 month")
all_months <- all_months[all_months >= ymd("1990-01-01")]

unique_crossections <- mclapply(all_months, compare_crossection, cshp=cshp, mc.cores = 16)
unique_crossections <- bind_rows(unique_crossections)


sections_with_change_plus_first <- filter(unique_crossections, change == 1 | unique_crossections$date == unique_crossections$date[1])

get_gwcode_rasters <- function(crossection_date, cshp){
  cshp_crossection <- cshp[crossection_date %within% cshp$date_interval,]


  cshp_crossection$date_interval <- NULL # Not supported by dplyr, so I remove the column

  cshp_pg <- st_intersection(pg_poly, cshp_crossection)
  cshp_pg$cell_area <- st_area(cshp_pg)

  #plot(cshp_pg["GWCODE"])

  cshp_pg_max <- group_by(cshp_pg, layer) %>%
    filter(cell_area == max(cell_area)) %>%
    ungroup()

  #plot(cshp_pg_max["GWCODE"])


  st_geometry(cshp_pg_max) <- NULL
  cshp_pg_max <- dplyr::select(cshp_pg_max, layer, GWCODE) %>%
    rename(pgid = layer,
           gwcode = GWCODE)

  class_df <- tibble(to = unique(pg[,]), from = unique(pg[,]))
  class_df <- left_join(class_df, cshp_pg_max, by = c("to" = "pgid"))

  classification_matrix <- as.matrix(class_df)
  gwcode <- reclassify(pg, classification_matrix, right = NA)
  #plot(gwcode)

  # This is only for one cross-section now. Should be yearly rasters, or similar.
  fname <- paste("data_raw/gwcode", crossection_date, ".rda", sep = "")
  save(gwcode, file = fname, compress = TRUE)
  return(crossection_date)
}

#ldf <- lapply(sections_with_change_plus_first$date[1], get_gwcode_rasters, cshp = cshp)

ldf <- mclapply(sections_with_change_plus_first$date, get_gwcode_rasters, cshp = cshp, mc.cores = 10)




