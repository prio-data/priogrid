# Generate PRIO-GRID Gross National Income per Capita

Convenience wrapper around
[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md) that
generates a PRIO-GRID–aligned raster of gross national income per
capita.

## Usage

``` r
gen_gnic(config = pg_current_config())
```

## Value

A `SpatRaster` object containing PRIO-GRID GNI per capita values.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`shdi`](http://prio-data.github.io/priogrid/reference/shdi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gnic_pg <- gen_gnic()
} # }
```
