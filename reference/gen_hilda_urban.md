# Extract Urban Coverage from HILDA+ Data

A convenience wrapper for
[`hilda_landcover`](http://prio-data.github.io/priogrid/reference/hilda_landcover.md)
that extracts urban coverage proportions (class code 11) for PRIO-GRID
cells.

## Usage

``` r
gen_hilda_urban(config = pg_current_config())
```

## Value

A `SpatRaster` with urban coverage proportions (0-1) for each PRIO-GRID
cell. See
[`hilda_landcover`](http://prio-data.github.io/priogrid/reference/hilda_landcover.md)
for details.

## See also

[`hilda_landcover`](http://prio-data.github.io/priogrid/reference/hilda_landcover.md)
for full documentation and parameters
