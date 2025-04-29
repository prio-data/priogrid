testthat::test_that("Setting up a new source works", {
  new_source <- Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "World",
    temporal_resolution = "Yearly",
    citation_keys = "schvitzMappingInternationalSystem2022; s",
    download_url = "www.example.com/path/to/my/new/source/file.csv",
    tags = "test"
  )

  testthat::expect_true(new_source$to_tibble()$website_url_exists) # www.example.com should exist
  testthat::expect_false(new_source$to_tibble()$download_url_exists) # this file does not exist
  testthat::expect_equal(new_source$to_tibble()$citation_keys, "schvitzMappingInternationalSystem2022") # s is not a valid bib-key

  new_source <- Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "World",
    temporal_resolution = "Yearly",
    citation_keys = "schvitzMappingInternationalSystem2022; s",
    download_url = "~/Downloads/urls_test.txt",
    tags = "test"
  )

  testthat::expect_error(Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "Yorld",
    temporal_resolution = "Yearly",
    citation_keys = "schvitzMappingInternationalSystem2022; s",
    download_url = "https://cdn.cloud.prio.org/files/bcf4115d-b742-464b-8b20-26a2d27a4ecb",
    tags = "test"
  )) # Yorld not valid spatial extent

  testthat::expect_error(Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "World",
    temporal_resolution = "Mearly",
    citation_keys = "schvitzMappingInternationalSystem2022; s",
    download_url = "https://cdn.cloud.prio.org/files/bcf4115d-b742-464b-8b20-26a2d27a4ecb",
    tags = "test"
  )) # Mearly not valid temporal resolution

  testthat::expect_error(Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "World",
    temporal_resolution = "Yearly",
    citation_keys = "schvitzMappingInternationalSystem2022; s",
    download_url = "www.example.com/path/to/my/new/source/file.csv",
    tags = "test; test2"
  )) # semi-colon separated tags!

  testthat::expect_error(Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "World",
    temporal_resolution = "Yearly",
    citation_keys = "schvitzMappingInternationalSystem2022, s",
    download_url = "www.example.com/path/to/my/new/source/file.csv",
    tags = "test"
  ))  # colon-separated citation_keys!

  testthat::expect_error(Source$new(
    source_name = "my new source",
    source_version = "v1.0",
    license = "CC BY 4.0",
    website_url = "www.example.com",
    spatial_extent = "World",
    temporal_resolution = "Yearly",
    citation_keys = "schvitzMappingInternationalSystem2022; s",
    reference_keys = "schvitzMappingInternationalSystem2022, s",
    download_url = "www.example.com/path/to/my/new/source/file.csv",
    tags = "test"
  ))  # colon-separated reference_keys!
})
