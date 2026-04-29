# Extract Cropland Coverage from HILDA+ Data

A convenience wrapper for
[`hilda_landcover`](http://prio-data.github.io/priogrid/reference/hilda_landcover.md)
that extracts cropland coverage proportions (class code 22) for
PRIO-GRID cells.

## Usage

``` r
gen_hilda_cropland(config = pg_current_config())
```

## Value

A `SpatRaster` with cropland coverage proportions (0-1) for each
PRIO-GRID cell. See
[`hilda_landcover`](http://prio-data.github.io/priogrid/reference/hilda_landcover.md)
for details.

## See also

[`hilda_landcover`](http://prio-data.github.io/priogrid/reference/hilda_landcover.md)
for full documentation and parameters
