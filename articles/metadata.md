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
#> [35] "gnic"
```

| Column       | Description                                                                                                                                                                                  |
|--------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`       | Variable name used in [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md), [`calc_pg()`](http://prio-data.github.io/priogrid/reference/calc_pg.md), etc. |
| `static`     | `TRUE` = no temporal dimension; `FALSE` = time-varying                                                                                                                                       |
| `source_ids` | Comma-separated UUIDs of the data sources feeding this variable                                                                                                                              |

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
#> [29] "gnic"
```

## Data Sources: `pgsources`

`pgsources` describes every raw data source that PRIOGRID draws on:

``` r
str(pgsources)
#> Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 50 obs. of  18 variables:
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

| Column                | Description                                             |
|-----------------------|---------------------------------------------------------|
| `id`                  | UUID — links to `pgvariables$source_ids`                |
| `source_name`         | Human-readable name (e.g., `"CRU TS"`, `"UCDP GED"`)    |
| `source_version`      | Data version used                                       |
| `license`             | Data license (e.g., `"CC BY 4.0"`)                      |
| `citation_keys`       | Semicolon-separated BibTeX keys → `inst/REFERENCES.bib` |
| `tags`                | Comma-separated tags (e.g., `"climate"`, `"conflict"`)  |
| `spatial_extent`      | `"World"`, `"Multiple continents"`, etc.                |
| `temporal_resolution` | `"Yearly"`, `"Monthly"`, `"Static"`, etc.               |
| `download_url`        | Primary download URL                                    |
| `website_url`         | Landing page URL                                        |
| `prio_mirror`         | PRIO-hosted mirror URL                                  |

Browse sources by tag or license:

``` r
# All sources with CC-BY license
pgsources[grepl("CC BY", pgsources$license), c("source_name", "license")]
#>                                               source_name         license
#> 1                       EOG Annual VIIRS Nighttime Lights       CC BY 4.0
#> 2                          SEDAC Food Insecurity Hotspots       CC BY 4.0
#> 3   Global Area Equipped for Irrigation Dataset 1900-2015       CC BY 4.0
#> 6                                            Li Nighttime       CC BY 4.0
#> 7      World Bank Global Subnational Poverty Atlas (GSAP)       CC BY 4.0
#> 9                                                UCDP GED       CC BY 4.0
#> 13             MCC-PIK DOSE – Subnational Economic Output       CC BY 4.0
#> 16                               WorldPop Migration Flows       CC BY 4.0
#> 17                                       GHSL GHS-BUILT-C       CC BY 4.0
#> 19                                       GHSL GHS-BUILT-H       CC BY 4.0
#> 21                                                 HILDA+       CC BY 4.0
#> 22                              GISCO Geostat Census Grid       CC BY 4.0
#> 23     GlobalDataLab Subnational Human Development (SHDI)        CC BY-NC
#> 24         Global Multi-resolution Terrain Elevation Data       CC BY 4.0
#> 27                                  Estimated Travel Time       CC BY 4.0
#> 28                                       GHSL GHS-BUILT-V       CC BY 4.0
#> 29                                 Global Irrigated Areas    CC BY-NC 3.0
#> 30                                   ORNL Landscan Global       CC BY 4.0
#> 32                               GHSL GHS Population Grid       CC BY 4.0
#> 34           UCDP Violent Political Protest Dataset (VPP)       CC BY 4.0
#> 36                               ReliefWeb Disasters List       CC BY 4.0
#> 37 World Bank Subnational Poverty and Inequality Database       CC BY 4.0
#> 38                                       GHSL GHS-BUILT-S       CC BY 4.0
#> 39                                     ESA WorldCover 10m       CC BY 4.0
#> 40          World Bank Subnational Doing Business Reports       CC BY 4.0
#> 41                                           GHSL GHS-DUC       CC BY 4.0
#> 42                                        ETH ICR cShapes CC BY-NC-SA 4.0
#> 43                         GHSL GHS Settlement Model Grid       CC BY 4.0
#> 44                                          geoBoundaries       CC BY 4.0
#> 49                                        GHS-WUP-DEGURBA       CC BY 4.0
#> 50                                               UCDP GED       CC BY 4.0

# Yearly time-varying sources
pgsources[pgsources$temporal_resolution == "Yearly", c("source_name", "source_version")]
#>                                               source_name source_version
#> 1                       EOG Annual VIIRS Nighttime Lights           V2.2
#> 4                             GlobalDataLab Area Database          v.4.2
#> 6                                            Li Nighttime             v8
#> 7      World Bank Global Subnational Poverty Atlas (GSAP)      Oct. 2024
#> 8                                        ETH ICR EPR Core           2023
#> 11                                         ETH ICR GeoEPR           2023
#> 12                            WIDE Education Inequalities           9.23
#> 13             MCC-PIK DOSE – Subnational Economic Output           v2.9
#> 15                   World Bank Geocoded Research Release          1.4.2
#> 21                                                 HILDA+           v1.0
#> 22                              GISCO Geostat Census Grid           2021
#> 23     GlobalDataLab Subnational Human Development (SHDI)          v.7.0
#> 29                                 Global Irrigated Areas           2018
#> 30                                   ORNL Landscan Global           2023
#> 31 SEDAC Global Gridded Relative Deprivation Index (GRDI)             v1
#> 33         GlobalDataLab International Wealth Index (IWI)          v.4.2
#> 34           UCDP Violent Political Protest Dataset (VPP)           20.1
#> 37 World Bank Subnational Poverty and Inequality Database       Oct 2024
#> 40          World Bank Subnational Doing Business Reports           2022
```

## File Integrity: `pgchecksum`

`pgchecksum` stores MD5 checksums for downloaded files, allowing you to
verify that your local copies match those used to build the official
release:

``` r
pgchecksum
#>                                               source_name source_version
#> 1                                         ETH ICR cShapes            2.0
#> 2                             IHME GHDx Under-5 mortality           2019
#> 3                    World Bank Geocoded Research Release          1.4.2
#> 4                                WorldPop Migration Flows           2019
#> 5                               GISCO Geostat Census Grid           2021
#> 6                          SEDAC Food Insecurity Hotspots             v1
#> 7                             WIDE Education Inequalities           9.23
#> 8  SEDAC Global Gridded Relative Deprivation Index (GRDI)             v1
#> 9                       Geocoded Disasters (GDIS) Dataset             v1
#> 10        SEDAC Global Subnational Infant Mortality Rates          v2.01
#> 11                               ReliefWeb Disasters List           2024
#> 12                                         ETH ICR GeoEPR           2021
#> 13                                               UCDP GED           24.1
#> 14             Natural Earth Breakaway and Disputed Areas          5.1.1
#> 15                        Natural Earth Physical 10m Land          5.1.1
#> 16                          FAO AQUASTAT Irrigation areas             v5
#> 17     World Bank Global Subnational Poverty Atlas (GSAP)      Oct. 2024
#> 18             MCC-PIK DOSE – Subnational Economic Output           v2.9
#> 19          World Bank Subnational Doing Business Reports           2022
#> 20 World Bank Subnational Poverty and Inequality Database       Oct 2024
#> 21 World Bank Subnational Poverty and Inequality Database       Oct 2024
#> 22 World Bank Subnational Poverty and Inequality Database       Oct 2024
#> 23                                       GHSL GHS-BUILT-S          R2023
#> 24                                       GHSL GHS-BUILT-S          R2023
#> 25                                       GHSL GHS-BUILT-S          R2023
#> 26                                       GHSL GHS-BUILT-S          R2023
#> 27                                       GHSL GHS-BUILT-S          R2023
#> 28                                       GHSL GHS-BUILT-S          R2023
#> 29                                       GHSL GHS-BUILT-S          R2023
#> 30                                       GHSL GHS-BUILT-S          R2023
#> 31                                       GHSL GHS-BUILT-S          R2023
#> 32                                       GHSL GHS-BUILT-S          R2023
#> 33                                       GHSL GHS-BUILT-S          R2023
#> 34                                       GHSL GHS-BUILT-S          R2023
#> 35                                       GHSL GHS-BUILT-V          R2023
#> 36                                       GHSL GHS-BUILT-V          R2023
#> 37                                       GHSL GHS-BUILT-V          R2023
#> 38                                       GHSL GHS-BUILT-V          R2023
#> 39                                       GHSL GHS-BUILT-V          R2023
#> 40                                       GHSL GHS-BUILT-V          R2023
#> 41                                       GHSL GHS-BUILT-V          R2023
#> 42                                       GHSL GHS-BUILT-V          R2023
#> 43                                       GHSL GHS-BUILT-V          R2023
#> 44                                       GHSL GHS-BUILT-V          R2023
#> 45                                       GHSL GHS-BUILT-V          R2023
#> 46                                       GHSL GHS-BUILT-V          R2023
#> 47                               GHSL GHS Population Grid          R2023
#> 48                               GHSL GHS Population Grid          R2023
#> 49                               GHSL GHS Population Grid          R2023
#> 50                               GHSL GHS Population Grid          R2023
#> 51                               GHSL GHS Population Grid          R2023
#> 52                               GHSL GHS Population Grid          R2023
#> 53                               GHSL GHS Population Grid          R2023
#> 54                               GHSL GHS Population Grid          R2023
#> 55                               GHSL GHS Population Grid          R2023
#> 56                               GHSL GHS Population Grid          R2023
#> 57                               GHSL GHS Population Grid          R2023
#> 58                               GHSL GHS Population Grid          R2023
#> 59                         GHSL GHS Settlement Model Grid          R2023
#> 60                         GHSL GHS Settlement Model Grid          R2023
#> 61                         GHSL GHS Settlement Model Grid          R2023
#> 62                         GHSL GHS Settlement Model Grid          R2023
#> 63                         GHSL GHS Settlement Model Grid          R2023
#> 64                         GHSL GHS Settlement Model Grid          R2023
#> 65                         GHSL GHS Settlement Model Grid          R2023
#> 66                         GHSL GHS Settlement Model Grid          R2023
#> 67                         GHSL GHS Settlement Model Grid          R2023
#> 68                         GHSL GHS Settlement Model Grid          R2023
#> 69                         GHSL GHS Settlement Model Grid          R2023
#> 70                         GHSL GHS Settlement Model Grid          R2023
#>                                      id
#> 1  ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b
#> 2  3868e499-5249-4582-958e-27de2b09945c
#> 3  52ac3e7e-b509-4d85-83b7-1875cb2b3afa
#> 4  5daf4962-3f07-408e-8e63-c1d7f8803070
#> 5  86532b44-ce5c-48a6-96f7-704885a9afb2
#> 6  0a746ab8-cc8e-4b31-bb71-8479a9ac8fa3
#> 7  4b61edd5-0d33-4a45-b0d3-757834c141ed
#> 8  a46019a1-4e3a-4cd0-81e6-eae6351b0415
#> 9  bdc773f4-7eb8-4f07-a4b5-663b8bc3f76e
#> 10 1e3634f6-267d-43c2-920e-34c9982e0a8d
#> 11 c1b411e0-5e6c-4b0f-9a4d-07e99f604ea9
#> 12 3900b527-a728-4c26-b0ab-f4441d3ee2e8
#> 13 2e5c66d2-d4e6-4282-9039-5b232b861093
#> 14 920663ad-d7e7-4528-b36d-4b7266def2b1
#> 15 92da9800-4520-4e87-a855-b28255452189
#> 16 514c2031-7216-4ac9-930d-ccb74ab2e73d
#> 17 2797f10a-a834-4f48-a6ea-3a1dbaf2e283
#> 18 4c471c6a-be5d-429a-8daa-3ac29b7ec36f
#> 19 e703f38e-5f1c-47c8-b798-e749ec503e98
#> 20 d8e6a15b-9353-42f5-8e79-8fa5da9428bc
#> 21 d8e6a15b-9353-42f5-8e79-8fa5da9428bc
#> 22 d8e6a15b-9353-42f5-8e79-8fa5da9428bc
#> 23 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 24 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 25 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 26 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 27 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 28 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 29 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 30 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 31 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 32 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 33 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 34 e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89
#> 35 9e85ae0c-c773-4636-a614-3933903e848c
#> 36 9e85ae0c-c773-4636-a614-3933903e848c
#> 37 9e85ae0c-c773-4636-a614-3933903e848c
#> 38 9e85ae0c-c773-4636-a614-3933903e848c
#> 39 9e85ae0c-c773-4636-a614-3933903e848c
#> 40 9e85ae0c-c773-4636-a614-3933903e848c
#> 41 9e85ae0c-c773-4636-a614-3933903e848c
#> 42 9e85ae0c-c773-4636-a614-3933903e848c
#> 43 9e85ae0c-c773-4636-a614-3933903e848c
#> 44 9e85ae0c-c773-4636-a614-3933903e848c
#> 45 9e85ae0c-c773-4636-a614-3933903e848c
#> 46 9e85ae0c-c773-4636-a614-3933903e848c
#> 47 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 48 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 49 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 50 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 51 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 52 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 53 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 54 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 55 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 56 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 57 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 58 ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 59 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 60 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 61 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 62 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 63 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 64 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 65 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 66 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 67 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 68 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 69 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#> 70 f37f3b1c-3b16-48e4-8aa3-7162b35a8096
#>                                                                                                                                                         filename
#> 1                                                                                   ETH ICR cShapes/2.0/ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b/CShapes-2.0.geojson
#> 2                                           IHME GHDx Under-5 mortality/2019/3868e499-5249-4582-958e-27de2b09945c/IHME_AFRICA_U5M_1998_2017_UNDER_5_GEO_TIFF.zip
#> 3                            World Bank Geocoded Research Release/1.4.2/52ac3e7e-b509-4d85-83b7-1875cb2b3afa/WorldBank_GeocodedResearchRelease_Level1_v1.4.2.zip
#> 4                                                              WorldPop Migration Flows/2019/5daf4962-3f07-408e-8e63-c1d7f8803070/SexDisaggregated_Migration.zip
#> 5                                                         GISCO Geostat Census Grid/2021/86532b44-ce5c-48a6-96f7-704885a9afb2/Eurostat_Census-GRID_2021_V2-0.zip
#> 6                             SEDAC Food Insecurity Hotspots/v1/0a746ab8-cc8e-4b31-bb71-8479a9ac8fa3/food-food-insecurity-hotspots-inputs-geographic-geotiff.zip
#> 7                                                            WIDE Education Inequalities/9.23/4b61edd5-0d33-4a45-b0d3-757834c141ed/1699460825-wide_2023_sept.csv
#> 8                                      SEDAC Global Gridded Relative Deprivation Index (GRDI)/v1/a46019a1-4e3a-4cd0-81e6-eae6351b0415/povmap-grdi-v1-geotiff.zip
#> 9                                      Geocoded Disasters (GDIS) Dataset/v1/bdc773f4-7eb8-4f07-a4b5-663b8bc3f76e/pend-gdis-1960-2018-disasterlocations-rdata.zip
#> 10 SEDAC Global Subnational Infant Mortality Rates/v2.01/1e3634f6-267d-43c2-920e-34c9982e0a8d/povmap-global-subnational-infant-mortality-rates-v2-01-geotiff.zip
#> 11                                                               ReliefWeb Disasters List/2024/c1b411e0-5e6c-4b0f-9a4d-07e99f604ea9/reliefweb-disasters-list.csv
#> 12                                                                                  ETH ICR GeoEPR/2021/3900b527-a728-4c26-b0ab-f4441d3ee2e8/GeoEPR-2021.geojson
#> 13                                                                                             UCDP GED/24.1/2e5c66d2-d4e6-4282-9039-5b232b861093/ged241-rds.zip
#> 14                             Natural Earth Breakaway and Disputed Areas/5.1.1/920663ad-d7e7-4528-b36d-4b7266def2b1/ne_50m_admin_0_breakaway_disputed_areas.zip
#> 15                                                                    Natural Earth Physical 10m Land/5.1.1/92da9800-4520-4e87-a855-b28255452189/ne_10m_land.zip
#> 16                                                                 FAO AQUASTAT Irrigation areas/v5/514c2031-7216-4ac9-930d-ccb74ab2e73d/gmia_v5_aei_pct_asc.zip
#> 17            World Bank Global Subnational Poverty Atlas (GSAP)/Oct. 2024/2797f10a-a834-4f48-a6ea-3a1dbaf2e283/AM24%20-%20GSAP%20data%202010%202019%202021.xlsx
#> 18                                                            MCC-PIK DOSE – Subnational Economic Output/v2.9/4c471c6a-be5d-429a-8daa-3ac29b7ec36f/DOSE_V2.9.csv
#> 19                             World Bank Subnational Doing Business Reports/2022/e703f38e-5f1c-47c8-b798-e749ec503e98/Historical-subnational-database-2022.xlsx
#> 20                                          World Bank Subnational Poverty and Inequality Database/Oct 2024/d8e6a15b-9353-42f5-8e79-8fa5da9428bc/AM24_MASTER.ZIP
#> 21                      World Bank Subnational Poverty and Inequality Database/Oct 2024/d8e6a15b-9353-42f5-8e79-8fa5da9428bc/AM24%20-%20SPID%20all%20groups.xlsx
#> 22                        World Bank Subnational Poverty and Inequality Database/Oct 2024/d8e6a15b-9353-42f5-8e79-8fa5da9428bc/AM24%20-%20Subnational%20MPM.xlsx
#> 23                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2030_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 24                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2025_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 25                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 26                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2015_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 27                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2010_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 28                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2005_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 29                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E2000_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 30                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E1995_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 31                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E1990_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 32                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E1985_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 33                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E1980_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 34                                                  GHSL GHS-BUILT-S/R2023/e59ea65b-8a6b-4f60-aa8f-6c53f1e78e89/GHS_BUILT_S_E1975_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 35                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2030_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 36                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2025_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 37                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 38                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2015_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 39                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2010_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 40                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2005_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 41                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E2000_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 42                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E1995_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 43                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E1990_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 44                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E1985_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 45                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E1980_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 46                                                  GHSL GHS-BUILT-V/R2023/9e85ae0c-c773-4636-a614-3933903e848c/GHS_BUILT_V_E1975_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 47                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2030_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 48                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2025_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 49                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 50                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2015_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 51                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2010_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 52                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2005_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 53                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E2000_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 54                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E1995_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 55                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E1990_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 56                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E1985_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 57                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E1980_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 58                                              GHSL GHS Population Grid/R2023/ae6a7612-4bef-452f-acd6-d2212cf9a7c5/GHS_POP_E1975_GLOBE_R2023A_4326_3ss_V1_0.zip
#> 59                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0.zip
#> 60                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V2_0.zip
#> 61                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.zip
#> 62                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2015_GLOBE_R2023A_54009_1000_V2_0.zip
#> 63                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2010_GLOBE_R2023A_54009_1000_V2_0.zip
#> 64                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2005_GLOBE_R2023A_54009_1000_V2_0.zip
#> 65                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E2000_GLOBE_R2023A_54009_1000_V2_0.zip
#> 66                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E1995_GLOBE_R2023A_54009_1000_V2_0.zip
#> 67                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E1990_GLOBE_R2023A_54009_1000_V2_0.zip
#> 68                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E1985_GLOBE_R2023A_54009_1000_V2_0.zip
#> 69                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E1980_GLOBE_R2023A_54009_1000_V2_0.zip
#> 70                                     GHSL GHS Settlement Model Grid/R2023/f37f3b1c-3b16-48e4-8aa3-7162b35a8096/GHS_SMOD_E1975_GLOBE_R2023A_54009_1000_V2_0.zip
#>                                 md5
#> 1  39d8daa630edd5e803db33f0531834c0
#> 2  5cfe5e82d63dae32789cb268ecfbcc30
#> 3  0477b2d21c1a1a01fff677ad634ce965
#> 4  f2ea5907082391376270a2a70c28be5e
#> 5  5adc394e050278c2722dc2604abad8cd
#> 6  32a6b528b6838ed105118632158b0670
#> 7  6055b7f87e59fe030fe12c7e7db9322f
#> 8  b8a101f06314ab076ec501a125c3e95a
#> 9  b35ffa42a03baecc33447b180f89db1b
#> 10 3891e95caa04e8903024856d1ba22e19
#> 11 56a4b859edcf38fa41354801bbb7f9ff
#> 12 0be53272bde0c59b371f17d7aea5b388
#> 13 3bced575397fc8c313f284599d7038fc
#> 14 e40b361feb30d5c78999029dce11f245
#> 15 be3001f37196d2894e17aacd13ff2cc2
#> 16 f54514aaf4fef7c90609921e3bb66d0b
#> 17 ce260ebb683b89d929faae424bff7524
#> 18 a62d060b755863cd4451f686f01ac7bb
#> 19 91b69be2ad338dbed4b664a727d9d4e4
#> 20 ea650d2a59e9af93936d2bbaf55d00ab
#> 21 c4865c26f1bffb9dd3b55a22d8e29bae
#> 22 6d08564986442bfce16d035ff67f03d1
#> 23 d93a8e150ac71ff2de116527e378ebb9
#> 24 dce28505dc9b483b60cb8237a449a68e
#> 25 f7bb34110076de010fea1210cf231b42
#> 26 5aaae9f2999de3034b74f96f6c501bc8
#> 27 6b6d2697636012402d4a6e104db80a25
#> 28 a61bc34a8f41df72e7995210bd7e0e6b
#> 29                             <NA>
#> 30 9b8a9dca05e7752e891770257262a930
#> 31                             <NA>
#> 32                             <NA>
#> 33 c49efeb058dba4f2ee782439d5bc8e6c
#> 34                             <NA>
#> 35 038ab95df66adc5d7c87ddad74f5f409
#> 36 10b1433f1eae17c8fa4b7359f9aab6c9
#> 37                             <NA>
#> 38                             <NA>
#> 39                             <NA>
#> 40                             <NA>
#> 41                             <NA>
#> 42                             <NA>
#> 43                             <NA>
#> 44                             <NA>
#> 45                             <NA>
#> 46 63e65985e6fc3f8ad3f8f781eb6e243e
#> 47 38bb5aa5146a5a122adf0984ffe9014b
#> 48 75d3ee17dcd7576ab426dc089d59959b
#> 49                             <NA>
#> 50                             <NA>
#> 51                             <NA>
#> 52 9061454855f860632f098d5e23d43c0a
#> 53                             <NA>
#> 54                             <NA>
#> 55 63605c7507e8f3570d26901c4d34228c
#> 56                             <NA>
#> 57 c529f884a1b31a55e20c70def61cbbc8
#> 58                             <NA>
#> 59                             <NA>
#> 60                             <NA>
#> 61 36cbc67335ac6d52c866c7c34e33c84e
#> 62 8167c17dbf03e4d1bfddd848a7458291
#> 63                             <NA>
#> 64 c7e45413ecb338d128fbf0d1998bb7ae
#> 65                             <NA>
#> 66                             <NA>
#> 67 95bf4aebe00608d536676f88a6c0530d
#> 68                             <NA>
#> 69 2b8ae4a397ecf14c5a2d1b3bc8efceea
#> 70 164933823de4044c756ad58237f5786f
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
#> 45 CRU Climate tmp          v4.09 harrisVersion4CRU2020
#>                                                                       license
#> 45 https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

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
#> # A tibble: 46 × 2
#>    source_name                                           spatial_extent
#>    <chr>                                                 <chr>         
#>  1 EOG Annual VIIRS Nighttime Lights                     World         
#>  2 Global Area Equipped for Irrigation Dataset 1900-2015 World         
#>  3 GlobalDataLab Area Database                           World         
#>  4 SEDAC Global Subnational Infant Mortality Rates       World         
#>  5 Li Nighttime                                          World         
#>  6 World Bank Global Subnational Poverty Atlas (GSAP)    World         
#>  7 ETH ICR EPR Core                                      World         
#>  8 UCDP GED                                              World         
#>  9 ETH ICR GeoEPR                                        World         
#> 10 WIDE Education Inequalities                           World         
#> # ℹ 36 more rows
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
#>   source_name                                        source_version filename    
#>   <chr>                                              <chr>          <chr>       
#> 1 SEDAC Food Insecurity Hotspots                     v1             SEDAC Food …
#> 2 SEDAC Global Subnational Infant Mortality Rates    v2.01          SEDAC Globa…
#> 3 Li Nighttime                                       v8             Li Nighttim…
#> 4 World Bank Global Subnational Poverty Atlas (GSAP) Oct. 2024      World Bank …
#> 5 ETH ICR EPR Core                                   2023           ETH ICR EPR…
#> 6 UCDP GED                                           24.1           UCDP GED/24…
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
