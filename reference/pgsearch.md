# Search PRIO-GRID meta-data

Use regex to search the meta-data for the data you are interested in.

## Usage

``` r
pgsearch(search_string, bib_element = NULL)
```

## Arguments

- search_string:

  A character string to search in source name, source version, source
  id, source tags, spatial extent, temporal resolution, or in
  bibliography elements.

- bib_element:

  Supports author, journal, year, or title. If null, the search will not
  search bibliography elements.

## Value

list with data.frames

## Examples

``` r
pgsearch("GHSL")
#> $in_name
#> # A tibble: 7 × 18
#>   id      source_name source_version license citation_keys aws_bucket aws_region
#>   <chr>   <chr>       <chr>          <chr>   <chr>         <chr>      <chr>     
#> 1 60eee3… GHSL GHS-B… R2023          CC BY … pesaresiGHSB… NA         NA        
#> 2 78d689… GHSL GHS-B… R2023          CC BY … pesaresiGHSB… NA         NA        
#> 3 9e85ae… GHSL GHS-B… R2023          CC BY … pesaresiGHSB… NA         NA        
#> 4 ae6a76… GHSL GHS P… R2023          CC BY … schiavinaGHS… NA         NA        
#> 5 e59ea6… GHSL GHS-B… R2023          CC BY … pesaresiGHSB… NA         NA        
#> 6 ea215b… GHSL GHS-D… R2023          CC BY … schiavinaGHS… NA         NA        
#> 7 f37f3b… GHSL GHS S… R2023          CC BY … schiavinaGHS… NA         NA        
#> # ℹ 11 more variables: download_url <chr>, website_url <chr>, tags <chr>,
#> #   spatial_extent <chr>, temporal_resolution <chr>, reference_keys <chr>,
#> #   prio_mirror <chr>, download_url_exists <lgl>, website_url_exists <lgl>,
#> #   prio_mirror_exists <lgl>, created_at <dttm>
#> 
#> $in_version
#> # A tibble: 0 × 18
#> # ℹ 18 variables: id <chr>, source_name <chr>, source_version <chr>,
#> #   license <chr>, citation_keys <chr>, aws_bucket <chr>, aws_region <chr>,
#> #   download_url <chr>, website_url <chr>, tags <chr>, spatial_extent <chr>,
#> #   temporal_resolution <chr>, reference_keys <chr>, prio_mirror <chr>,
#> #   download_url_exists <lgl>, website_url_exists <lgl>,
#> #   prio_mirror_exists <lgl>, created_at <dttm>
#> 
#> $in_id
#> # A tibble: 0 × 18
#> # ℹ 18 variables: id <chr>, source_name <chr>, source_version <chr>,
#> #   license <chr>, citation_keys <chr>, aws_bucket <chr>, aws_region <chr>,
#> #   download_url <chr>, website_url <chr>, tags <chr>, spatial_extent <chr>,
#> #   temporal_resolution <chr>, reference_keys <chr>, prio_mirror <chr>,
#> #   download_url_exists <lgl>, website_url_exists <lgl>,
#> #   prio_mirror_exists <lgl>, created_at <dttm>
#> 
#> $in_tags
#> # A tibble: 0 × 18
#> # ℹ 18 variables: id <chr>, source_name <chr>, source_version <chr>,
#> #   license <chr>, citation_keys <chr>, aws_bucket <chr>, aws_region <chr>,
#> #   download_url <chr>, website_url <chr>, tags <chr>, spatial_extent <chr>,
#> #   temporal_resolution <chr>, reference_keys <chr>, prio_mirror <chr>,
#> #   download_url_exists <lgl>, website_url_exists <lgl>,
#> #   prio_mirror_exists <lgl>, created_at <dttm>
#> 
#> $in_spatial_extent
#> # A tibble: 0 × 18
#> # ℹ 18 variables: id <chr>, source_name <chr>, source_version <chr>,
#> #   license <chr>, citation_keys <chr>, aws_bucket <chr>, aws_region <chr>,
#> #   download_url <chr>, website_url <chr>, tags <chr>, spatial_extent <chr>,
#> #   temporal_resolution <chr>, reference_keys <chr>, prio_mirror <chr>,
#> #   download_url_exists <lgl>, website_url_exists <lgl>,
#> #   prio_mirror_exists <lgl>, created_at <dttm>
#> 
#> $in_temporal_resolution
#> # A tibble: 0 × 18
#> # ℹ 18 variables: id <chr>, source_name <chr>, source_version <chr>,
#> #   license <chr>, citation_keys <chr>, aws_bucket <chr>, aws_region <chr>,
#> #   download_url <chr>, website_url <chr>, tags <chr>, spatial_extent <chr>,
#> #   temporal_resolution <chr>, reference_keys <chr>, prio_mirror <chr>,
#> #   download_url_exists <lgl>, website_url_exists <lgl>,
#> #   prio_mirror_exists <lgl>, created_at <dttm>
#> 
```
