new_source <- Source$new( source_name = "GLC_FCS30",
                          source_version = "v2",
                          license = "CC BY 4.0",
                          website_url = "https://zenodo.org/records/15063683",
                          spatial_extent = "World",
                          temporal_resolution = "Yearly",
                          citation_keys = "zhang_glc_fcs30_2021", # Semi-colon separated if multiple
                          download_url = "/Users/gbenz/Documents/priogrid/pg_git/GLC_Processing/GLC_FC30_v2_downloadlinks.txt",
                          tags = "landcover")

add_source(new_source)
