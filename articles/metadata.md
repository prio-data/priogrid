# Understanding PRIOGRID Metadata

PRIOGRID embeds rich metadata about every variable and data source. This
tutorial walks through the three core metadata objects — `pgvariables`,
`pgsources`, and `pgchecksum` — and the search and discovery tools built
on top of them.

## Variables: `pgvariables`

`pgvariables` is a data frame listing every variable PRIOGRID can
calculate:

``` r

pgvariables$name
#>  [1] "cru_tmp"                      "cru_pre"                     
#>  [3] "cru_pet"                      "cshapes_cover_share"         
#>  [5] "cshapes_gwcode"               "geoepr_reg_excluded"         
#>  [7] "bdist1"                       "bdist2"                      
#>  [9] "bdist3"                       "ghsl_population_grid"        
#> [11] "hilda_cropland"               "hilda_forest"                
#> [13] "hilda_grassland"              "hilda_ocean"                 
#> [15] "hilda_pasture"                "hilda_sparse"                
#> [17] "hilda_urban"                  "hilda_water"                 
#> [19] "linight_mean"                 "naturalearth_cover"          
#> [21] "naturalearth_cover_share"     "ruggedterrain_elevation_mean"
#> [23] "traveltime_mean"              "traveltime_min"              
#> [25] "geopko_troops_count"          "geopko_operations_count"     
#> [27] "ne_disputed_area_share"       "speibase6_mean"              
#> [29] "ghs_wup_degurba_urban"        "ucdp_ged"                    
#> [31] "shdi"                         "msch"                        
#> [33] "esch"                         "lifexp"                      
#> [35] "gnic"                         "side_excluded"               
#> [37] "side_included"                "side_irrelevant"
```

| Column | Description |
|----|----|
| `name` | Variable name used in [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md), [`calc_pg()`](http://prio-data.github.io/priogrid/reference/calc_pg.md), etc. |
| `static` | `TRUE` = no temporal dimension; `FALSE` = time-varying |
| `source_ids` | Comma-separated UUIDs of the data sources feeding this variable |
| `label` | Human-readable display title for plots/legends |
| `unit` | Unit string (e.g. `°C`, `mm`); empty if dimensionless |
| `transform` | Display transform: `identity`, `log1p`, `log10`, or `sqrt` |
| `plot_type` | One of: `continuous`, `positive_real`, `count`, `share`, `discrete` |

The four authored display columns (`label`, `unit`, `transform`,
`plot_type`) are stamped into each built COG as `pg_*` GDAL metatags by
[`save_pgvariable()`](http://prio-data.github.io/priogrid/reference/save_pgvariable.md).
Built COGs additionally carry computed
`pg_colormap`/`pg_value_min`/`pg_value_max`/`pg_value_mean`/`pg_value_std`/`pg_nunique`/`pg_class_values`
(not authored columns). These map onto rio-tiler
`rescale`=`pg_value_min`,`pg_value_max` and
`colormap_name`=`pg_colormap`.

Static and time-varying variables:

``` r

pgvariables[pgvariables$static == TRUE,  "name"]  # terrain, borders, etc.
#> [1] "naturalearth_cover"           "naturalearth_cover_share"    
#> [3] "ruggedterrain_elevation_mean" "traveltime_mean"             
#> [5] "traveltime_min"               "ne_disputed_area_share"
pgvariables[pgvariables$static == FALSE, "name"]  # climate, conflict, etc.
#>  [1] "cru_tmp"                 "cru_pre"                
#>  [3] "cru_pet"                 "cshapes_cover_share"    
#>  [5] "cshapes_gwcode"          "geoepr_reg_excluded"    
#>  [7] "bdist1"                  "bdist2"                 
#>  [9] "bdist3"                  "ghsl_population_grid"   
#> [11] "hilda_cropland"          "hilda_forest"           
#> [13] "hilda_grassland"         "hilda_ocean"            
#> [15] "hilda_pasture"           "hilda_sparse"           
#> [17] "hilda_urban"             "hilda_water"            
#> [19] "linight_mean"            "geopko_troops_count"    
#> [21] "geopko_operations_count" "speibase6_mean"         
#> [23] "ghs_wup_degurba_urban"   "ucdp_ged"               
#> [25] "shdi"                    "msch"                   
#> [27] "esch"                    "lifexp"                 
#> [29] "gnic"                    "side_excluded"          
#> [31] "side_included"           "side_irrelevant"
```

## Data Sources: `pgsources`

`pgsources` describes every raw data source that PRIOGRID draws on:

``` r

str(pgsources)
#> Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 52 obs. of  18 variables:
#>  $ id                 : chr  "04254b82-85f0-4c06-9f1b-86ed35e05403" "0a746ab8-cc8e-4b31-bb71-8479a9ac8fa3" "1604221b-e558-4e65-b7fe-d6b0a517ff5c" "190c7abc-b779-4462-97ff-00994cbd2431" ...
#>  $ source_name        : chr  "EOG Annual VIIRS Nighttime Lights" "SEDAC Food Insecurity Hotspots" "Global Area Equipped for Irrigation Dataset 1900-2015" "GlobalDataLab Area Database" ...
#>  $ source_version     : chr  "V2.2" "v1" "v4" "v.4.2" ...
#>  $ license            : chr  "CC BY 4.0" "CC BY 4.0" "CC BY 4.0" "https://globaldatalab.org/termsofuse/" ...
#>  $ citation_keys      : chr  "elvidgeAnnualTimeSeries2021" "centerforinternationalearthscienceinformationnetworkciesincolumbiauniversityFoodInsecurityHotspots2020" "mehtaHalfTwentyfirstCentury2024" "smitsGDLAreaDatabase2016; globaldatalabAreaDatabase2015" ...
#>  $ aws_bucket         : chr  NA NA NA NA ...
#>  $ aws_region         : chr  NA NA NA NA ...
#>  $ download_url       : chr  NA "https://sedac.ciesin.columbia.edu/downloads/data/food/food-food-insecurity-hotspots/food-food-insecurity-hotspo"| __truncated__ NA NA ...
#>  $ website_url        : chr  "https://eogdata.mines.edu/products/vnl/#annual_v2" "https://sedac.ciesin.columbia.edu/data/set/food-food-insecurity-hotspots" "https://zenodo.org/records/14219723" "https://globaldatalab.org/areadata/" ...
#>  $ tags               : chr  "demographic, remote - sense, socio - economic" "demographic, food insecurity" "irrigation, land use" "infrastructure, sanitation" ...
#>  $ spatial_extent     : chr  "World" "Several countries (spread)" "World" "World" ...
#>  $ temporal_resolution: chr  "Yearly" "Quarterly" "Less than yearly" "Yearly" ...
#>  $ reference_keys     : chr  "elvidgeVIIRSNighttimeLights2017" "omayProjectedFutureChanges2024" NA NA ...
#>  $ prio_mirror        : chr  NA NA NA NA ...
#>  $ download_url_exists: logi  NA FALSE NA NA FALSE FALSE ...
#>  $ website_url_exists : logi  NA TRUE NA NA TRUE TRUE ...
#>  $ prio_mirror_exists : logi  NA NA NA NA NA FALSE ...
#>  $ created_at         : POSIXct, format: "2024-12-04 08:35:28" "2024-12-03 14:00:14" ...
#>  - attr(*, "spec")=List of 3
#>   ..$ cols   :List of 18
#>   .. ..$ id                 : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ source_name        : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ source_version     : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ license            : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ citation_keys      : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ aws_bucket         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ aws_region         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ download_url       : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ website_url        : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ tags               : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ spatial_extent     : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ temporal_resolution: list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ reference_keys     : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ prio_mirror        : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ download_url_exists: list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
#>   .. ..$ website_url_exists : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
#>   .. ..$ prio_mirror_exists : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
#>   .. ..$ created_at         :List of 1
#>   .. .. ..$ format: chr ""
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_datetime" "collector"
#>   ..$ default: list()
#>   .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
#>   ..$ delim  : chr "\t"
#>   ..- attr(*, "class")= chr "col_spec"
#>  - attr(*, "problems")=<pointer: (nil)>
```

Key columns:

| Column | Description |
|----|----|
| `id` | UUID — links to `pgvariables$source_ids` |
| `source_name` | Human-readable name (e.g., `"CRU TS"`, `"UCDP GED"`) |
| `source_version` | Data version used |
| `license` | Data license (e.g., `"CC BY 4.0"`) |
| `citation_keys` | Semicolon-separated BibTeX keys → `inst/REFERENCES.bib` |
| `tags` | Comma-separated tags (e.g., `"climate"`, `"conflict"`) |
| `spatial_extent` | `"World"`, `"Multiple continents"`, etc. |
| `temporal_resolution` | `"Yearly"`, `"Monthly"`, `"Static"`, etc. |
| `download_url` | Primary download URL |
| `website_url` | Landing page URL |
| `prio_mirror` | PRIO-hosted mirror URL |

Browse sources by tag or license:

``` r

# All sources with CC-BY license
pgsources[grepl("CC BY", pgsources$license), c("source_name", "license")]
#>                                               source_name         license
#> 1                       EOG Annual VIIRS Nighttime Lights       CC BY 4.0
#> 2                          SEDAC Food Insecurity Hotspots       CC BY 4.0
#> 3   Global Area Equipped for Irrigation Dataset 1900-2015       CC BY 4.0
#> 6                                            Li Nighttime       CC BY 4.0
#> 7                                            Li Nighttime       CC BY 4.0
#> 8      World Bank Global Subnational Poverty Atlas (GSAP)       CC BY 4.0
#> 10                                               UCDP GED       CC BY 4.0
#> 14             MCC-PIK DOSE – Subnational Economic Output       CC BY 4.0
#> 17                               WorldPop Migration Flows       CC BY 4.0
#> 18                                       GHSL GHS-BUILT-C       CC BY 4.0
#> 20                                       GHSL GHS-BUILT-H       CC BY 4.0
#> 22                                                 HILDA+       CC BY 4.0
#> 23                              GISCO Geostat Census Grid       CC BY 4.0
#> 24     GlobalDataLab Subnational Human Development (SHDI)        CC BY-NC
#> 25         Global Multi-resolution Terrain Elevation Data       CC BY 4.0
#> 28                                  Estimated Travel Time       CC BY 4.0
#> 29                                       GHSL GHS-BUILT-V       CC BY 4.0
#> 30                                 Global Irrigated Areas    CC BY-NC 3.0
#> 31                                   ORNL Landscan Global       CC BY 4.0
#> 33                               GHSL GHS Population Grid       CC BY 4.0
#> 35           UCDP Violent Political Protest Dataset (VPP)       CC BY 4.0
#> 37                               ReliefWeb Disasters List       CC BY 4.0
#> 38 World Bank Subnational Poverty and Inequality Database       CC BY 4.0
#> 39                                       GHSL GHS-BUILT-S       CC BY 4.0
#> 40                                     ESA WorldCover 10m       CC BY 4.0
#> 41          World Bank Subnational Doing Business Reports       CC BY 4.0
#> 42                                           GHSL GHS-DUC       CC BY 4.0
#> 43                                        ETH ICR cShapes CC BY-NC-SA 4.0
#> 44                         GHSL GHS Settlement Model Grid       CC BY 4.0
#> 45                                          geoBoundaries       CC BY 4.0
#> 50                                        GHS-WUP-DEGURBA       CC BY 4.0
#> 51                                               UCDP GED       CC BY 4.0
#> 52                                               ETH SIDE       CC BY 4.0

# Yearly time-varying sources
pgsources[pgsources$temporal_resolution == "Yearly", c("source_name", "source_version")]
#>                                               source_name source_version
#> 1                       EOG Annual VIIRS Nighttime Lights           V2.2
#> 4                             GlobalDataLab Area Database          v.4.2
#> 6                                            Li Nighttime             v8
#> 7                                            Li Nighttime            v10
#> 8      World Bank Global Subnational Poverty Atlas (GSAP)      Oct. 2024
#> 9                                        ETH ICR EPR Core           2023
#> 12                                         ETH ICR GeoEPR           2023
#> 13                            WIDE Education Inequalities           9.23
#> 14             MCC-PIK DOSE – Subnational Economic Output           v2.9
#> 16                   World Bank Geocoded Research Release          1.4.2
#> 22                                                 HILDA+           v1.0
#> 23                              GISCO Geostat Census Grid           2021
#> 24     GlobalDataLab Subnational Human Development (SHDI)          v.7.0
#> 30                                 Global Irrigated Areas           2018
#> 31                                   ORNL Landscan Global           2023
#> 32 SEDAC Global Gridded Relative Deprivation Index (GRDI)             v1
#> 34         GlobalDataLab International Wealth Index (IWI)          v.4.2
#> 35           UCDP Violent Political Protest Dataset (VPP)           20.1
#> 38 World Bank Subnational Poverty and Inequality Database       Oct 2024
#> 41          World Bank Subnational Doing Business Reports           2022
#> 52                                               ETH SIDE             v1
```

## File Integrity: `pgchecksum`

`pgchecksum` stores MD5 checksums for downloaded files, allowing you to
verify that your local copies match those used to build the official
release:

``` r

head(pgchecksum)
#>                                       source_name source_version
#> 1                  SEDAC Food Insecurity Hotspots             v1
#> 2 SEDAC Global Subnational Infant Mortality Rates          v2.01
#> 3                                    Li Nighttime             v8
#> 4                                    Li Nighttime             v8
#> 5                                    Li Nighttime             v8
#> 6                                    Li Nighttime             v8
#>                                     id
#> 1 0a746ab8-cc8e-4b31-bb71-8479a9ac8fa3
#> 2 1e3634f6-267d-43c2-920e-34c9982e0a8d
#> 3 24d76a3b-927e-42ad-b8a5-2e7443e6a275
#> 4 24d76a3b-927e-42ad-b8a5-2e7443e6a275
#> 5 24d76a3b-927e-42ad-b8a5-2e7443e6a275
#> 6 24d76a3b-927e-42ad-b8a5-2e7443e6a275
#>                                                                                                                                                        filename
#> 1                            SEDAC Food Insecurity Hotspots/v1/0a746ab8-cc8e-4b31-bb71-8479a9ac8fa3/food-food-insecurity-hotspots-inputs-geographic-geotiff.zip
#> 2 SEDAC Global Subnational Infant Mortality Rates/v2.01/1e3634f6-267d-43c2-920e-34c9982e0a8d/povmap-global-subnational-infant-mortality-rates-v2-01-geotiff.zip
#> 3                                                                       Li Nighttime/v8/24d76a3b-927e-42ad-b8a5-2e7443e6a275/Harmonized_DN_NTL_1992_calDMSP.tif
#> 4                                                                       Li Nighttime/v8/24d76a3b-927e-42ad-b8a5-2e7443e6a275/Harmonized_DN_NTL_1993_calDMSP.tif
#> 5                                                                       Li Nighttime/v8/24d76a3b-927e-42ad-b8a5-2e7443e6a275/Harmonized_DN_NTL_1994_calDMSP.tif
#> 6                                                                       Li Nighttime/v8/24d76a3b-927e-42ad-b8a5-2e7443e6a275/Harmonized_DN_NTL_1995_calDMSP.tif
#>                                md5
#> 1 32a6b528b6838ed105118632158b0670
#> 2 3891e95caa04e8903024856d1ba22e19
#> 3 ccd2c6176493314969cdeb0ee7567b8f
#> 4 0c72818fb4aa41870fd8f3b4d2499b4b
#> 5 171b42e04c506b23f168707f133a37c6
#> 6 a77886218aa047931ce4610fcdab0435
```

Use
[`check_pgsourcefiles()`](http://prio-data.github.io/priogrid/reference/check_pgsourcefiles.md)
to run the check against your local storage:

``` r

check_pgsourcefiles()
# "All files in your local storage are similar to a tested set."
```

## Metadata Linking: Variables → Sources → Bibliography

The three metadata objects form a chain:

    pgvariables$source_ids → pgsources$id → pgsources$citation_keys → REFERENCES.bib

Here’s the full lookup for a single variable:

``` r

# 1. Find source IDs for a variable
var_row <- pgvariables[pgvariables$name == "cru_tmp", ]
source_ids <- strsplit(var_row$source_ids, ", ")[[1]]
source_ids
#> [1] "ac037134-3567-49d9-a3ba-64f37c1ee698"

# 2. Look up source metadata
src <- pgsources[pgsources$id %in% source_ids, c("source_name", "source_version", "citation_keys", "license")]
src
#>        source_name source_version         citation_keys
#> 46 CRU Climate tmp          v4.09 harrisVersion4CRU2020
#>                                                                       license
#> 46 https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

# 3. Get citation keys
bibkeys <- unlist(strsplit(src$citation_keys, "; "))
bibkeys
#> [1] "harrisVersion4CRU2020"
```

Then retrieve the full bibliography:

``` r

get_bibliography(bibkeys[1])
#> [1] I. Harris, T. J. Osborn, P. Jones, et al. "Version 4 of the CRU TS
#> Monthly High-Resolution Gridded Multivariate Climate Dataset". In:
#> _Scientific Data_ 7.1 (Apr. 2020), p. 109. ISSN: 2052-4463. DOI:
#> 10.1038/s41597-020-0453-3.
```

## Searching Metadata: `pgsearch()`

[`pgsearch()`](http://prio-data.github.io/priogrid/reference/pgsearch.md)
searches across all text fields in `pgsources` using regex:

``` r

results <- pgsearch("population")
```

It returns a named list with matches from each search field:

``` r

# Sources matching by name
results$in_name[, c("source_name", "source_version")]
#> # A tibble: 1 × 2
#>   source_name              source_version
#>   <chr>                    <chr>         
#> 1 GHSL GHS Population Grid R2023

# Sources matching by tag
results$in_tags[, c("source_name", "tags")]
#> # A tibble: 9 × 2
#>   source_name                                tags                               
#>   <chr>                                      <chr>                              
#> 1 ETH ICR EPR Core                           ethnicity, population, social stru…
#> 2 ETH ICR GeoEPR                             ethnicity, population, social stru…
#> 3 MCC-PIK DOSE – Subnational Economic Output population, socio - economic       
#> 4 GHSL GHS-BUILT-C                           demographic, infrastructure, land …
#> 5 GHSL GHS-BUILT-H                           demographic, land use, population  
#> 6 GHSL GHS Population Grid                   demographic, infrastructure, land …
#> 7 GHSL GHS-BUILT-S                           demographic, land use, population  
#> 8 GHSL GHS Settlement Model Grid             demographic, population, urbanizat…
#> 9 GHS-WUP-DEGURBA                            urban extent, population
```

Search by temporal resolution:

``` r

pgsearch("Monthly")$in_temporal_resolution[, c("source_name", "temporal_resolution")]
#> # A tibble: 9 × 2
#>   source_name                                  temporal_resolution
#>   <chr>                                        <chr>              
#> 1 UCDP GED                                     Higher than monthly
#> 2 Armed Conflict Location & Event Data (ACLED) Higher than monthly
#> 3 Geocoded Peacekeeping Operations (Geo-PKO)   Monthly            
#> 4 ETH ICR cShapes                              Higher than monthly
#> 5 CRU Climate tmp                              Monthly            
#> 6 CRU Climate pre                              Monthly            
#> 7 CRU Climate pet                              Monthly            
#> 8 Global SPEI database                         Monthly            
#> 9 UCDP GED                                     Higher than monthly
```

Search by spatial extent:

``` r

pgsearch("World")$in_spatial_extent[, c("source_name", "spatial_extent")]
#> # A tibble: 47 × 2
#>    source_name                                           spatial_extent
#>    <chr>                                                 <chr>         
#>  1 EOG Annual VIIRS Nighttime Lights                     World         
#>  2 Global Area Equipped for Irrigation Dataset 1900-2015 World         
#>  3 GlobalDataLab Area Database                           World         
#>  4 SEDAC Global Subnational Infant Mortality Rates       World         
#>  5 Li Nighttime                                          World         
#>  6 Li Nighttime                                          World         
#>  7 World Bank Global Subnational Poverty Atlas (GSAP)    World         
#>  8 ETH ICR EPR Core                                      World         
#>  9 UCDP GED                                              World         
#> 10 ETH ICR GeoEPR                                        World         
#> # ℹ 37 more rows
```

### Searching Bibliography Elements

Include `bib_element` to also search author names, titles, journals, or
years:

``` r

# Find sources citing a specific author
harris_results <- pgsearch("Harris", bib_element = "author")
#> No results.
#> No results.
#> No results.
#> No results.
#> No results.
#> No results.
#> No results.
harris_results$in_element[, c("source_name", "citation_keys")]
#> # A tibble: 3 × 2
#> # Rowwise: 
#>   source_name     citation_keys        
#>   <chr>           <chr>                
#> 1 CRU Climate tmp harrisVersion4CRU2020
#> 2 CRU Climate pre harrisVersion4CRU2020
#> 3 CRU Climate pet harrisVersion4CRU2020
```

## Listing Raw Files

[`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md)
returns a data frame of all files PRIOGRID can download, with their URLs
and expected local paths:

``` r

files <- pg_rawfiles()
head(files[, c("source_name", "source_version", "filename")])
#> # A tibble: 6 × 3
#>   source_name                                     source_version filename       
#>   <chr>                                           <chr>          <chr>          
#> 1 SEDAC Food Insecurity Hotspots                  v1             SEDAC Food Ins…
#> 2 SEDAC Global Subnational Infant Mortality Rates v2.01          SEDAC Global S…
#> 3 Li Nighttime                                    v8             Li Nighttime/v…
#> 4 Li Nighttime                                    v8             Li Nighttime/v…
#> 5 Li Nighttime                                    v8             Li Nighttime/v…
#> 6 Li Nighttime                                    v8             Li Nighttime/v…
```

Filter to a specific source:

``` r

files[files$source_name == "ETH ICR cShapes", c("source_name", "filename", "url")]
#> # A tibble: 1 × 3
#>   source_name     filename                                                 url  
#>   <chr>           <chr>                                                    <chr>
#> 1 ETH ICR cShapes ETH ICR cShapes/2.0/ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6… http…
```

Download a specific source:

``` r

ucdp_files <- pg_rawfiles() |> dplyr::filter(source_name == "UCDP GED")
download_pg_rawdata(file_info = ucdp_files)
```

Get the local path to a downloaded file:

``` r

get_pgfile(
  source_name    = "ETH ICR cShapes",
  source_version = "2.0",
  id             = "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b"
)
```

## Checking Data Availability

[`pg_data_availability()`](http://prio-data.github.io/priogrid/reference/pg_data_availability.md)
shows which sources have been downloaded locally:

``` r

pg_data_availability()
#   source_name   source_version n_files n_present all_present
# 1 CRU TS        4.08                1         1        TRUE
# 2 ETH ICR cShapes 2.0              1         1        TRUE
# ...
```

## Next Steps

- [Citations and
  Bibliography](http://prio-data.github.io/priogrid/articles/citation.md)
  — citing data providers in publications
- [Contributing to
  PRIOGRID](http://prio-data.github.io/priogrid/articles/contributing.md)
  — adding new sources and variables
