new_source <- Source$new(
  source_name = "GDL SHDI",
  source_version = "v4.2",
  license = "CC BY 4.0",
  website_url = "https://globaldatalab.org/areadata/",
  spatial_extent = "World",
  temporal_resolution = "Yearly",
  citation_keys = "globaldatalabSubnationalHumanDevelopment2019",
  prio_mirror = "https://cdn.cloud.prio.org/files/e3e16271-34a8-4a99-bef4-3bb0e3c917aa/Area%20Database%20Data%20v441.csv?inline=true",
  tags = "subnational, development, GDL"
)

add_source(new_source)

files_to_download <- pg_rawfiles() |> dplyr::filter(id == "68b56155-7248-4f6f-b4a7-b1fb13336dae")

download_pg_rawdata(file_info = files_to_download)