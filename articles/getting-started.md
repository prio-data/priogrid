# Getting Started with PRIOGRID

PRIOGRID is an R package for collecting and standardizing open spatial
data into a common grid format. This tutorial covers the basics: setting
up the package, downloading data, and reading it into R as tabular data
— no `terra` or `sf` required.

## Initial Setup

PRIOGRID stores raw downloaded data and processed outputs in a single
folder on your machine. This path persists across R sessions.

``` r
library(priogrid)

# Set the data folder (run once; persists across sessions)
pg_set_rawfolder("/path/to/your/data/folder")
```

Once set, you can retrieve it at any time:

``` r
pg_rawfolder()
```

## Configuration

PRIOGRID has a session-scoped configuration object that controls the
spatial and temporal parameters of the grid. The defaults match the
official PRIOGRID release:

``` r
pg_current_config()
#> PRIO-GRID config:
#>   nrow: 360 
#>   ncol: 720 
#>   crs: epsg:4326 
#>   extent: -180 180 -90 90 
#>   temporal_resolution: 1 year 
#>   start_date: 1850-12-31 
#>   end_date: 2026-04-29 
#>   verbose: TRUE 
#>   automatic_download: TRUE
```

| Parameter             | Default       | Meaning                      |
|-----------------------|---------------|------------------------------|
| `nrow` / `ncol`       | 360 / 720     | Grid dimensions (0.5° cells) |
| `crs`                 | `"epsg:4326"` | WGS84 geographic coordinates |
| `extent`              | global        | -180 to 180, -90 to 90       |
| `temporal_resolution` | `"1 year"`    | Annual time steps            |
| `start_date`          | `1850-12-31`  | First measurement date       |
| `end_date`            | today         | Last measurement date        |

You can inspect individual fields:

``` r
cfg <- pg_current_config()
cfg$nrow
#> [1] 360
cfg$temporal_resolution
#> [1] "1 year"
```

The configuration is only in effect for the current R session. See the
[Custom
Configurations](http://prio-data.github.io/priogrid/articles/custom-config.md)
vignette for how to change it.

## Downloading the Official Release

Download the current official PRIOGRID release with a single call. This
downloads a zip archive and extracts it to your raw data folder:

``` r
download_priogrid()
```

To see what releases are available:

``` r
download_priogrid(list_releases = TRUE)
```

The download only happens once — subsequent calls are skipped unless you
set `overwrite = TRUE`.

## Reading PRIOGRID Data

Once downloaded, load the full dataset as a `data.table`. This requires
no spatial libraries.

### Static Variables

Static variables do not change over time (e.g., terrain elevation,
country border distances):

``` r
pg_static <- read_pg_static()
head(pg_static)
```

Each row is a PRIOGRID cell identified by `pgid`. Columns are variable
names.

### Time-Varying Variables

Time-varying variables have a `measurement_date` column in addition to
`pgid`:

``` r
pg_tv <- read_pg_timevarying()
head(pg_tv)
```

These tables can be joined together by `pgid`, or merged with your own
data.

### Example: subsetting and merging

``` r
library(data.table)

# Subset to a specific year
pg_2020 <- pg_tv[measurement_date == as.Date("2020-12-31")]

# Join static and time-varying on pgid
pg_merged <- merge(pg_static, pg_2020, by = "pgid")
```

## Browsing Available Variables

The `pgvariables` data frame lists all variables in PRIOGRID:

``` r
pgvariables
#>                            name static
#> 1                       cru_tmp  FALSE
#> 2                       cru_pre  FALSE
#> 3                       cru_pet  FALSE
#> 4           cshapes_cover_share  FALSE
#> 5                cshapes_gwcode  FALSE
#> 6           geoepr_reg_excluded  FALSE
#> 7                        bdist1  FALSE
#> 8                        bdist2  FALSE
#> 9                        bdist3  FALSE
#> 10         ghsl_population_grid  FALSE
#> 11               hilda_cropland  FALSE
#> 12                 hilda_forest  FALSE
#> 13              hilda_grassland  FALSE
#> 14                  hilda_ocean  FALSE
#> 15                hilda_pasture  FALSE
#> 16                 hilda_sparse  FALSE
#> 17                  hilda_urban  FALSE
#> 18                  hilda_water  FALSE
#> 19                 linight_mean  FALSE
#> 20           naturalearth_cover   TRUE
#> 21     naturalearth_cover_share   TRUE
#> 22 ruggedterrain_elevation_mean   TRUE
#> 23              traveltime_mean   TRUE
#> 24               traveltime_min   TRUE
#> 25          geopko_troops_count  FALSE
#> 26      geopko_operations_count  FALSE
#> 27       ne_disputed_area_share   TRUE
#> 28               speibase6_mean  FALSE
#> 29        ghs_wup_degurba_urban  FALSE
#> 30                     ucdp_ged  FALSE
#> 31                         shdi  FALSE
#> 32                         msch  FALSE
#> 33                         esch  FALSE
#> 34                       lifexp  FALSE
#> 35                         gnic  FALSE
#>                                                                                                          source_ids
#> 1                                                                              ac037134-3567-49d9-a3ba-64f37c1ee698
#> 2                                                                              00575260-ad1c-4e87-a575-3922bc151f50
#> 3                                                                              95399c70-7db4-47f0-95e5-2e279b6b2054
#> 4                                                                              ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b
#> 5                                                                              ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b
#> 6                                        287bfdf7-2f4f-402a-88df-5fe1f8b7046b, 3900b527-a728-4c26-b0ab-f4441d3ee2e8
#> 7                                                                              ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b
#> 8                                                                              ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b
#> 9                                                                              ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b
#> 10                                                                             ae6a7612-4bef-452f-acd6-d2212cf9a7c5
#> 11                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 12                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 13                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 14                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 15                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 16                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 17                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 18                                                                             82bc4c6f-9904-484f-aa9a-77771d076690
#> 19                                                                             24d76a3b-927e-42ad-b8a5-2e7443e6a275
#> 20                                                                             92da9800-4520-4e87-a855-b28255452189
#> 21                                                                             92da9800-4520-4e87-a855-b28255452189
#> 22                                                                             8c8192eb-cc29-4598-8f8a-ec190ba35c2d
#> 23                                                                             9aa052f6-4d04-4ed1-9eed-e47e08828d38
#> 24                                                                             9aa052f6-4d04-4ed1-9eed-e47e08828d38
#> 25                                                                             7dcbfbfb-9667-4684-af34-85f69fa8d0a0
#> 26                                                                             7dcbfbfb-9667-4684-af34-85f69fa8d0a0
#> 27                                                                             920663ad-d7e7-4528-b36d-4b7266def2b1
#> 28 14839384-623a-4cf7-9241-6166a8ac465b, 95399c70-7db4-47f0-95e5-2e279b6b2054, 00575260-ad1c-4e87-a575-3922bc151f50
#> 29                                                                             7f1f60a3-6664-4427-b086-b5359ebf45b7
#> 30                                                                             49f79d96-4e4d-4812-9dd1-862bacfca577
#> 31 8aaf6b27-6372-43da-87a9-d4235095bb2c, a8e35e36-9f7e-4194-9cc4-ce8ca59f7b51, 8aaf6b27-6372-43da-87a9-d4235095bb2c
#> 32 8aaf6b27-6372-43da-87a9-d4235095bb2c, a8e35e36-9f7e-4194-9cc4-ce8ca59f7b51, 8aaf6b27-6372-43da-87a9-d4235095bb2c
#> 33 8aaf6b27-6372-43da-87a9-d4235095bb2c, a8e35e36-9f7e-4194-9cc4-ce8ca59f7b51, 8aaf6b27-6372-43da-87a9-d4235095bb2c
#> 34 8aaf6b27-6372-43da-87a9-d4235095bb2c, a8e35e36-9f7e-4194-9cc4-ce8ca59f7b51, 8aaf6b27-6372-43da-87a9-d4235095bb2c
#> 35 8aaf6b27-6372-43da-87a9-d4235095bb2c, a8e35e36-9f7e-4194-9cc4-ce8ca59f7b51, 8aaf6b27-6372-43da-87a9-d4235095bb2c
```

The `static` column indicates whether a variable varies over time. Use
this to filter:

``` r
# Static variables
pgvariables[pgvariables$static == TRUE, "name"]
#> [1] "naturalearth_cover"           "naturalearth_cover_share"    
#> [3] "ruggedterrain_elevation_mean" "traveltime_mean"             
#> [5] "traveltime_min"               "ne_disputed_area_share"

# Time-varying variables
pgvariables[pgvariables$static == FALSE, "name"]
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

## Rawfolder Structure

After downloading, your data folder has this structure:

    {rawfolder}/
    ├── priogrid/
    │   ├── priogrid_3_0_1_05deg_yearly.zip   # Downloaded archive
    │   └── releases/
    │       └── 3.0.1/
    │           └── 05deg_yearly/
    │               ├── pg_static.parquet        # Static data (cached)
    │               ├── pg_timevarying.parquet   # Time-varying data (cached)
    │               └── {varname}.rds            # Individual variable rasters
    ├── {source_name}/{version}/{id}/            # Raw source files
    └── tmp/                                     # Temporary processing files

The `.parquet` files are what
[`read_pg_static()`](http://prio-data.github.io/priogrid/reference/read_pg_static.md)
and
[`read_pg_timevarying()`](http://prio-data.github.io/priogrid/reference/read_pg_timevarying.md)
read by default. The `.rds` files contain individual variables as
rasters (used by
[`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md)).

## Next Steps

- [Accessing PRIOGRID as
  Rasters](http://prio-data.github.io/priogrid/articles/working-with-rasters.md)
  — using `terra` to work with individual variables as spatial rasters
- [Citations and
  Bibliography](http://prio-data.github.io/priogrid/articles/citation.md)
  — how to cite data providers
- [Custom
  Configurations](http://prio-data.github.io/priogrid/articles/custom-config.md)
  — changing resolution, extent, and time period
- [Understanding PRIOGRID
  Metadata](http://prio-data.github.io/priogrid/articles/metadata.md) —
  exploring `pgsources`, `pgvariables`, and
  [`pgsearch()`](http://prio-data.github.io/priogrid/reference/pgsearch.md)
