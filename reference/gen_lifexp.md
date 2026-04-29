# Generate PRIO-GRID Life Expectancy

Convenience wrapper around
[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md) that
generates a PRIO-GRID–aligned raster of life expectancy at birth.

## Usage

``` r
gen_lifexp(config = pg_current_config())
```

## Value

A `SpatRaster` object containing PRIO-GRID life expectancy values.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lifexp_pg <- gen_lifexp()
} # }
```
