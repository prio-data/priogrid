

pgmeta <- readxl::read_excel("data_raw/pgmeta_raw.xlsx")

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
dplyr::copy_to(con, pgmeta)
pgmeta2 <- dplyr::tbl(con, "pgmeta")

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
  "cite" = "schvitzMappingInternationalSystem2022",
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
  "src_name" = "NaturalEarth10mLand",
  "version" = "4.1.1",
  "cite" = "",
  "references" = list(c("")),
  "license" = "Public Domain",
  "spatial_resolution" = "",
  "spatial_extent" = list(c(-180, 180, -90, 90)),
  "crs" = "",
  "temporal_extent" = list(c()),
  "data_url" = "https://naturalearth.s3.amazonaws.com/4.1.1/10m_physical/ne_10m_land.zip",
  "prio_mirror_url" = "",
  "status" = "survey"
)

pgmeta <- dplyr::bind_rows(cshapes, natural_earth)

usethis::use_data(pgmeta, overwrite = TRUE)
