# Accessing PRIOGRID as Rasters

This tutorial covers working with PRIOGRID data as spatial rasters using
the `terra` package. Rasters are useful for visualization, spatial
analysis, and working with individual variables before converting to
tabular form.

All raster-related functions in PRIOGRID require `terra`:

``` r
install.packages("terra")
library(terra)
```

## The Reference Grid

[`prio_blank_grid()`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)
creates an empty `SpatRaster` filled with PRIOGRID cell IDs (`pgid`).
This is useful to understand the grid geometry and to attach values for
plotting:

``` r
pg <- prio_blank_grid()
pg
# class       : SpatRaster
# dimensions  : 360, 720, 1  (nrow, ncol, nlyr)
# resolution  : 0.5, 0.5  (x, y)
# extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326)
# source(s)   : memory
# name        : pgid

terra::plot(pg)
```

## Loading a Single Variable

[`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md)
downloads (if needed) and returns any PRIOGRID variable as a
`SpatRaster`. By default it uses the official release:

``` r
ucdp <- load_pgvariable("ucdp_ged")
ucdp
```

Each layer corresponds to one time step. Layer names are dates in
`"YYYY-MM-DD"` format:

``` r
names(ucdp)[1:5]
# [1] "1989-12-31" "1990-12-31" "1991-12-31" "1992-12-31" "1993-12-31"
```

Select a single layer by name:

``` r
ucdp_2020 <- ucdp[["2020-12-31"]]
terra::plot(log1p(ucdp_2020), main = "UCDP GED events (log scale), 2020")
```

## Loading Static Variables as Rasters

Static variables (no time dimension) are returned as a named list of
single-layer `SpatRaster` objects:

``` r
static_rasters <- read_pg_static(as_raster = TRUE)
names(static_rasters)

# Plot terrain ruggedness
terra::plot(static_rasters[["ruggedterrain_elevation_mean"]])
```

## Loading Time-Varying Variables as Rasters

Time-varying variables are returned as a named list, each element being
a multi-layer `SpatRaster`:

``` r
tv_rasters <- read_pg_timevarying(as_raster = TRUE)

# Each element is a SpatRaster with one layer per year
tv_rasters[["cru_tmp"]]
```

## Discovering Variables with pgsearch()

[`pgsearch()`](http://prio-data.github.io/priogrid/reference/pgsearch.md)
searches the meta-data across source names, versions, tags, spatial
extent, and temporal resolution:

``` r
results <- pgsearch("climate")
# Returns a named list of data frames for each search category

# Which sources match by tag?
results$in_tags[, c("source_name", "tags")]
#> # A tibble: 4 × 2
#>   source_name          tags                                   
#>   <chr>                <chr>                                  
#> 1 CRU Climate tmp      climate, validation, temperature       
#> 2 CRU Climate pre      climate, validation, precipitation     
#> 3 CRU Climate pet      climate, validation, evapotranspiration
#> 4 Global SPEI database drought index, climate
```

Search by tag to find related variables:

``` r
conflict_sources <- pgsearch("conflict")$in_tags
conflict_sources[, c("source_name", "temporal_resolution")]
#> # A tibble: 3 × 2
#>   source_name                                  temporal_resolution
#>   <chr>                                        <chr>              
#> 1 UCDP GED                                     Higher than monthly
#> 2 Armed Conflict Location & Event Data (ACLED) Higher than monthly
#> 3 UCDP GED                                     Higher than monthly
```

## Converting Rasters Back to Data Frames

[`rast_to_df()`](http://prio-data.github.io/priogrid/reference/rast_to_df.md)
converts a `SpatRaster` to a `data.table` with `pgid` (and
`measurement_date` for time-varying data):

``` r
r <- load_pgvariable("ruggedterrain_elevation_mean")
df <- rast_to_df(r, static = TRUE, varname = "ruggedterrain_elevation_mean")
head(df)
#    pgid  ruggedterrain_elevation_mean
# 1:    1                           NA
# 2:    2                           NA
# ...
```

For time-varying data, set `static = FALSE`:

``` r
r <- load_pgvariable("cru_tmp")
df <- rast_to_df(r, static = FALSE, varname = "cru_tmp")
head(df)
#    pgid measurement_date  cru_tmp
# 1:    1       1901-12-31       NA
```

## Combining Rasters with terra

`terra` supports stacking, arithmetic, and many other operations. Here
are a few patterns:

``` r
# Stack two variables for combined analysis
elev <- load_pgvariable("ruggedterrain_elevation_mean")
pop  <- load_pgvariable("ghsl_population_grid")[["2020-12-31"]]

# Crop to a region (e.g., Sub-Saharan Africa)
africa_ext <- terra::ext(c(-20, 55, -35, 40))
elev_africa <- terra::crop(elev, africa_ext)
pop_africa  <- terra::crop(pop, africa_ext)

terra::plot(c(elev_africa, log1p(pop_africa)),
            main = c("Elevation", "Population (log)"))
```

## Saving and Reusing Custom Results

[`save_pgvariable()`](http://prio-data.github.io/priogrid/reference/save_pgvariable.md)
saves any `SpatRaster` back to the PRIOGRID data folder as a wrapped
`.rds` file:

``` r
r <- gen_ruggedterrain_elevation_mean()
save_pgvariable(r, "ruggedterrain_elevation_mean")

# Later, load it back:
r2 <- load_pgvariable("ruggedterrain_elevation_mean")
```

## Next Steps

- [Custom
  Configurations](http://prio-data.github.io/priogrid/articles/custom-config.md)
  — building variables at custom resolutions and extents
- [Understanding PRIOGRID
  Metadata](http://prio-data.github.io/priogrid/articles/metadata.md) —
  exploring `pgsources` and `pgvariables`
