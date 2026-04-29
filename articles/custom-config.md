# Custom Spatial and Temporal Configurations

PRIOGRID’s default release uses 0.5-degree global cells with annual time
steps. This tutorial shows how to change the spatial extent, resolution,
projection, and temporal granularity — useful for testing the modifiable
areal unit problem (MAUP), creating tailored regional datasets, or
generating monthly or quarterly data.

## The Configuration Object

All spatial and temporal parameters are held in a `pg_config` object.
The current session config is accessed with:

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

Create a custom config with
[`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md):

``` r
cfg <- pg_config(
  nrow = 180,
  ncol = 360,
  crs  = "epsg:4326",
  extent = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
  temporal_resolution = "1 year",
  start_date = as.Date("1990-12-31"),
  end_date   = as.Date("2020-12-31")
)
cfg
#> PRIO-GRID config:
#>   nrow: 180 
#>   ncol: 360 
#>   crs: epsg:4326 
#>   extent: -180 180 -90 90 
#>   temporal_resolution: 1 year 
#>   start_date: 1990-12-31 
#>   end_date: 2020-12-31 
#>   verbose: TRUE 
#>   automatic_download: TRUE
```

| Parameter                 | Type       | Description                                  |
|---------------------------|------------|----------------------------------------------|
| `nrow` / `ncol`           | integer    | Grid dimensions                              |
| `crs`                     | character  | Any PROJ/EPSG string                         |
| `extent`                  | numeric(4) | `c(xmin, xmax, ymin, ymax)`                  |
| `temporal_resolution`     | character  | `"1 year"`, `"1 month"`, `"1 quarter"`, etc. |
| `start_date` / `end_date` | Date       | First and last measurement dates             |
| `verbose`                 | logical    | Print progress messages                      |
| `automatic_download`      | logical    | Auto-download missing source files           |

> **Date convention:** Use the last day of each temporal increment. For
> yearly data with December measurement dates, use `1990-12-31`. For
> monthly data with January measurement dates, use `1990-01-31`.

## Updating the Session Config

[`pg_set_config()`](http://prio-data.github.io/priogrid/reference/pg_set_config.md)
modifies the current session config in place:

``` r
pg_set_config(
  temporal_resolution = "1 month",
  start_date = as.Date("2000-01-31"),
  end_date   = as.Date("2005-12-31")
)
pg_current_config()$temporal_resolution
#> [1] "1 month"
```

Only the fields you specify are changed. Reset to defaults at any time:

``` r
pg_reset_config()
pg_current_config()$temporal_resolution  # back to "1 year"
#> [1] "1 year"
```

## Spatial and Temporal Hashes

Custom builds are stored on disk using two 6-character MD5 hashes:

- **Spatial hash** — derived from `nrow`, `ncol`, `crs`, and `extent`
- **Temporal hash** — derived from `temporal_resolution`, `start_date`,
  and `end_date`

These hashes uniquely identify a configuration without encoding the full
parameter set in the folder name. The file path looks like:

    {rawfolder}/priogrid/custom/{pkg_version}/{spatial_hash}/{temporal_hash}/

For example, the default config produces:

``` r
# Internal helpers (not exported, shown for illustration)
cfg_default <- pg_config()
cat("Spatial hash:", priogrid:::get_spatial_hash(cfg_default), "\n")
#> Spatial hash: 244f73
cat("Temporal hash:", priogrid:::get_temporal_hash(cfg_default), "\n")
#> Temporal hash: 6ffd78
```

If you change any spatial parameter, the spatial hash changes and
PRIOGRID will treat it as a new, independent build. This prevents
accidental mixing of data from incompatible configurations.

> **Important:** PRIOGRID cell IDs (`pgid`) depend on the spatial
> configuration. Data from different configs cannot be joined by `pgid`.

## Where Custom Data Is Stored

[`pgout_path()`](http://prio-data.github.io/priogrid/reference/pgout_path.md)
returns the storage path for the current config:

``` r
cfg <- pg_config(nrow = 180, ncol = 360)
pgout_path(config = cfg)
# {rawfolder}/priogrid/custom/3.0.1/a1b2c3/d4e5f6/
```

For the official release:

``` r
pgout_path(version = "3.0.1", type = "05deg_yearly")
# {rawfolder}/priogrid/releases/3.0.1/05deg_yearly/
```

## Building Custom Variables

[`calc_pg()`](http://prio-data.github.io/priogrid/reference/calc_pg.md)
calculates variables and saves them to disk. It calls the `gen_*()`
function for each variable:

``` r
cfg <- pg_config(
  nrow = 180,
  ncol = 360,
  start_date = as.Date("2000-12-31"),
  end_date   = as.Date("2020-12-31")
)

# Calculate a single variable
calc_pg("ruggedterrain_elevation_mean", config = cfg)

# Calculate multiple variables
calc_pg(c("ruggedterrain_elevation_mean", "naturalearth_cover"), config = cfg)

# Calculate all variables (takes a long time)
calc_pg(config = cfg)
```

By default, existing files are skipped. Use `overwrite = TRUE` to
recalculate:

``` r
calc_pg("ruggedterrain_elevation_mean", overwrite = TRUE, config = cfg)
```

After calculation, load results with
[`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md):

``` r
r <- load_pgvariable("ruggedterrain_elevation_mean", config = cfg)
terra::plot(r)
```

## Example: Monthly Temperature for Europe

``` r
cfg_europe <- pg_config(
  nrow   = 100,
  ncol   = 140,
  extent = c(xmin = -25, xmax = 45, ymin = 34, ymax = 72),
  temporal_resolution = "1 month",
  start_date = as.Date("2010-01-31"),
  end_date   = as.Date("2020-12-31")
)

calc_pg("cru_tmp", config = cfg_europe)

r <- load_pgvariable("cru_tmp", config = cfg_europe)
terra::plot(r[["2015-07-31"]], main = "Mean temperature, July 2015 (Europe)")
```

## Example: Custom Projection

``` r
# Lambert Azimuthal Equal-Area, centred on Asia
cfg_laea <- pg_config(
  nrow = 200,
  ncol = 200,
  crs  = "+proj=laea +lon_0=90 +lat_0=45 +datum=WGS84 +units=m"
)

calc_pg("ruggedterrain_elevation_mean", config = cfg_laea)
r <- load_pgvariable("ruggedterrain_elevation_mean", config = cfg_laea)
terra::plot(r)
```

## Listing Custom Builds

[`pg_list_custom()`](http://prio-data.github.io/priogrid/reference/pg_list_custom.md)
shows all custom builds currently stored on disk:

``` r
customs <- pg_list_custom()
# [1] a1b2c3/d4e5f6: nrow=180, ncol=360, 2000-12-31 to 2020-12-31, 2 vars, created 2025-01-15
# [2] e7f8g9/h0i1j2: nrow=100, ncol=140, 2010-01-31 to 2020-12-31, 1 vars, created 2025-01-16

# Load data from a listed config
read_pg_static(config = customs[[1]])
```

## Reading Custom Data as Tables

Load custom builds into tabular form the same way as the official
release:

``` r
cfg <- pg_config(nrow = 180, ncol = 360)

pg_static_custom <- read_pg_static(config = cfg)
pg_tv_custom     <- read_pg_timevarying(config = cfg)
```

## Release vs. Custom Mode Summary

| Feature                                                                                 | Release mode (`config = NULL`) | Custom mode (`config = cfg`)                                                           |
|-----------------------------------------------------------------------------------------|--------------------------------|----------------------------------------------------------------------------------------|
| Source                                                                                  | Official PRIO CDN              | Local computation                                                                      |
| Path                                                                                    | `releases/{version}/{type}/`   | `custom/{version}/{s_hash}/{t_hash}/`                                                  |
| Grid                                                                                    | Fixed (0.5°, global, yearly)   | Any resolution / extent / CRS                                                          |
| `pgid`                                                                                  | Official IDs                   | Config-dependent IDs                                                                   |
| [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md) | Auto-downloads                 | Requires [`calc_pg()`](http://prio-data.github.io/priogrid/reference/calc_pg.md) first |

## Next Steps

- [Understanding PRIOGRID
  Metadata](http://prio-data.github.io/priogrid/articles/metadata.md) —
  sources, variables, and search
- [Contributing to
  PRIOGRID](http://prio-data.github.io/priogrid/articles/contributing.md)
  — writing `gen_*()` functions and registering new sources
