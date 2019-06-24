library(sf)
library(raster)
library(dplyr)
library(ncdf4)

setwd("E:/Dropbox/Papers/ClimateSensitiveConflictProjections/")

mirca <- read.table(gzfile("data/mirca2000/CELL_SPECIFIC_CROPPING_CALENDARS.TXT.gz"), header=T, sep="\t")

m_small <- mirca %>% group_by(lat, long, start, end) %>%
  summarize(area = sum(area)) %>%
  ungroup() %>%
  group_by(lat, long) %>%
  filter(area == max(area))

m_raster <- rasterFromXYZ(m_small[c("long", "lat", "start", "end", "area")], crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0", digits=1)

writeRaster(m_raster, c("data/mirca2000/mirca2000_max_start.nc",
                        "data/mirca2000/mirca2000_max_end.nc",
                        "data/mirca2000/mirca2000_max_area.nc"), 
            overwrite=TRUE, format="CDF", bylayer = TRUE, force_v4=TRUE, compression=7,
            NAflag=-1, xname="lon", yname="lat", zname="variable")