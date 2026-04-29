# Generate PRIO-GRID SHDI

Convenience wrapper around
[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md) that
generates a PRIO-GRID–aligned raster of the Subnational Human
Development Index (SHDI).

## Usage

``` r
gen_shdi(config = pg_current_config())
```

## Value

A `SpatRaster` object containing PRIO-GRID SHDI values.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
shdi_pg <- gen_shdi()
} # }
```
