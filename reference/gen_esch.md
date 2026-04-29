# Generate PRIO-GRID Expected Years of Schooling

Convenience wrapper around
[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md) that
generates a PRIO-GRID–aligned raster of expected years of schooling.

## Usage

``` r
gen_esch(config = pg_current_config())
```

## Value

A `SpatRaster` object containing PRIO-GRID expected years of schooling.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
esch_pg <- gen_esch()
} # }
```
