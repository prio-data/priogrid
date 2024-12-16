
# <- dplyr::tibble(
#   "src_name" = "",
#   "version" = "",
#   "cite" = "",
#   "references" = list(c("")),
#   "license" = "",
#   "spatial_resolution" = "",
#   "spatial_extent" = list(c(-180, 180, -90, 90)),
#   "crs" = "",
#   "temporal_extent" = list(c()),
#   "data_url" = "",
#   "prio_mirror_url" = "",
#   "status" = "survey"
# )


cshapes <- dplyr::tibble(
  "src_name" = "CShapes",
  "version" = "2.0",
  "cite" = "some_bib_key",
  "references" = list(c("other", "bib", "keys")),
  "license" = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
  "spatial_resolution" = "vector",
  "spatial_extent" = list(c(-180, 180, -55.90223, 83.11387)),
  "crs" = "epsg:4326",
  "temporal_extent" = list(c(1886, 2024)),
  "data_url" = "https://icr.ethz.ch/data/cshapes/CShapes-2.0.geojson",
  "prio_mirror_url" = "",
  "status" = "staging"
)

natural_earth <- dplyr::tibble(
  "src_name" = "NaturalEarth50mLand",
  "version" = "4.1.1",
  "cite" = "",
  "references" = list(c("")),
  "license" = "Public Domain",
  "spatial_resolution" = "",
  "spatial_extent" = list(c(-180, 180, -90, 90)),
  "crs" = "",
  "temporal_extent" = list(c()),
  "data_url" = "https://naturalearth.s3.amazonaws.com/4.1.1/50m_physical/ne_50m_land.zip",
  "prio_mirror_url" = "",
  "status" = "survey"
)

geostat <- dplyr::tibble(
  "src_name" = "GeostatCencusGrid",
  "version" = "2021",
  "cite" = "some_bib_key",
  "references" = list(c("other", "bib", "keys")),
  "license" = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
  "spatial_resolution" = "Raster",
  "spatial_extent" = list(c(-180, 180, -55.90223, 83.11387)),
  "crs" = "epsg:4326",
  "temporal_extent" = list(c(1886, 2024)),
  "data_url" = "https://gisco-services.ec.europa.eu/census/2021/Eurostat_Census-GRID_2021_V2-0.zip",
  "prio_mirror_url" = "",
  "status" = "staging"
)

geo_pko <- dplyr::tibble(
  "src_name" = "Geo-PKO",
  "version" = "2.1",
  "cite" = "some_bib_key",
  "references" = list(c("other", "bib", "keys")),
  "license" = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
  "spatial_resolution" = "Raster",
  "spatial_extent" = list(c(-180, 180, 90, 90)),
  "crs" = "epsg:4326",
  "temporal_extent" = list(c(1994, 2020)),
  "data_url" = "https://www.uu.se/download/18.24f546b0193ac54c49a1af2b/1734079168492/c_818704-l_1-k_geo_pko_v.2.2.xlsx",
  "prio_mirror_url" = "",
  "status" = "staging"
  )

pgmeta <- dplyr::bind_rows(cshapes, natural_earth, geostat, geo_pko)

usethis::use_data(pgmeta, overwrite = TRUE)
