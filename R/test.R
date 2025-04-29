library(terra)
library(sf)
library(dplyr)
library(readr)

# --- Source PRIOGRID base files ---
source("~/Desktop/Gitprodjects/priogrid/R/utility.R")
source("~/Desktop/Gitprodjects/priogrid/R/options.R")

# --- Initialize PRIO-GRID options ---
pgoptions <- PGOptionsManager$new()
pgoptions$set_ncol(720)
pgoptions$set_nrow(360)
pgoptions$set_extent(c(-180, 180, -90, 90))
pgoptions$set_crs("EPSG:4326")

shapefile <- "/Users/ingvildsmestad/Desktop/Gitprodjects/GDL Shapefiles V6/GDL Shapefiles V6.4 large.shp"
hdi_geom <- sf::read_sf(shapefile)

shdi_globaldatalab <- read_csv("/Users/ingvildsmestad/Desktop/Gitprodjects/GDL-Subnational-HDI-data.csv") %>%
  rename(gdlcode = GDLCODE, shdi = `2010`) %>%  # Random
  select(gdlcode, shdi)

shdi_globaldatalab$gdlcode <- tolower(shdi_globaldatalab$gdlcode)
hdi_geom$gdlcode <- tolower(hdi_geom$gdlcode)

hdi_geom <- left_join(hdi_geom, shdi_globaldatalab, by = "gdlcode")

hdi_geom <- hdi_geom %>% filter(!is.na(shdi))

hdi_geom <- sf::st_make_valid(hdi_geom)

hdi_geom <- sf::st_simplify(hdi_geom, dTolerance = 1)

gen_shdi_priogrid <- function(hdi_geom) {
  pg <- prio_blank_grid()

  if (sf::st_crs(hdi_geom)$wkt != terra::crs(pg)) {
    message("Transforming CRS...")
    hdi_geom <- sf::st_transform(hdi_geom, crs = terra::crs(pg))
  }

  message("Converting to terra::vect...")
  hdi_vect <- terra::vect(hdi_geom)

  message("Rasterizing...")
  shdi_raster <- terra::rasterize(hdi_vect, pg, field = "shdi", fun = "mean")

  names(shdi_raster) <- "shdi"
  return(shdi_raster)
}

# --- Generate SHDI raster ---
start_time <- Sys.time()
shdi_raster <- gen_shdi_priogrid(hdi_geom)
end_time <- Sys.time()

print(paste("Time to generate SHDI raster:", round(as.numeric(end_time - start_time, units = "secs"), 2), "seconds"))

# --- Plot the result ---
plot(shdi_raster)

# --- Save raster to file ---
# Save as .rds
#saveRDS(shdi_raster, file = "shdi_raster.rds")

# Save as GeoTIFF
# terra::writeRaster(shdi_raster, "shdi_raster.tif", overwrite = TRUE)
