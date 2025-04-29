
shapefile <- "/Users/ingvildsmestad/Desktop/Gitprodjects/GDL Shapefiles V6/GDL Shapefiles V6.4 large.shp"
hdi_geom <- sf::read_sf(shapefile)

shdi_globaldatalab <- read_csv("/Users/ingvildsmestad/Desktop/Gitprodjects/GDL-Subnational-HDI-data.csv") %>%
  rename(gdlcode = GDLCODE, shdi = `2010`) %>% #example
  select(gdlcode, shdi)

shdi_globaldatalab$gdlcode <- tolower(shdi_globaldatalab$gdlcode)
shdi_geom$gdlcode <- tolower(shdi_geom$gdlcode)

shdi_geom <- left_join(shdi_geom, shdi_globaldatalab, by = "gdlcode")

gen_shdi_priogrid <- function(shdi_geom) {
  pg <- prio_blank_grid()

  if (sf::st_crs(shdi_geom) != terra::crs(pg)) {
    shdi_geom <- sf::st_transform(shdi_geom, sf::st_crs(pg))
  }

  shdi_raster <- terra::rasterize(shdi_geom, pg, field = "shdi", fun = mean)
  names(shdi_raster) <- "shdi"

  return(shdi_raster)
}



