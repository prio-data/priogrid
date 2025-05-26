new_source <- Source$new(
  source_name = "GDL SHDI",
  source_version = "v4.2",
  license = "CC BY 4.0",
  website_url = "https://globaldatalab.org/areadata/",
  spatial_extent = "World",
  temporal_resolution = "Yearly",
  citation_keys = "globaldatalabSubnationalHumanDevelopment2019",
  download_url = "https://cdn.cloud.prio.org/files/e3e16271-34a8-4a99-bef4-3bb0e3c917aa/Area%20Database%20Data%20v441.csv?inline=true",
  tags = "subnational, development, GDL"
)

add_source(new_source)

pgsources <- readr::read_tsv("data_raw/sources.csv", show_col_types = FALSE)
usethis::use_data(pgsources, overwrite = TRUE)
