# Generate the average (mean) elevation for PRIO-GRID cells

A convenience wrapper function that calculates mean elevation values for
each PRIO-GRID cell using
[`ruggedterrain_variable`](http://prio-data.github.io/priogrid/reference/ruggedterrain_variable.md).

## Usage

``` r
gen_ruggedterrain_elevation_mean(config = pg_current_config())
```

## Value

A `SpatRaster` object

## References

Danielson JJ, Gesch DB (2011). “Global Multi-resolution Terrain
Elevation Data 2010 (GMTED2010).” Technical Report 2011-1073, Earth
Resources Observation and Science (EROS) Center, Virginia.

## See also

[`ruggedterrain_variable`](http://prio-data.github.io/priogrid/reference/ruggedterrain_variable.md)
for the underlying function that supports min, max, and mean elevation
calculations

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate mean elevation raster
mean_elevation <- gen_ruggedterrain_elevation_mean()
} # }
```
