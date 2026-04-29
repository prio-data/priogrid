# Read GlobalDataLab SHDI Shapefile Data

Downloads from a PRIO hosted mirror.

## Usage

``` r
read_shdi_shapefile()
```

## Value

An `sf` object containing SHDI geometries with GlobalDataLab subnational
identifiers.

## Details

This function reads the GlobalDataLab Subnational Human Development
Index (SHDI) shapefile dataset (version 7.0) from local storage. The
shapefile is extracted from a compressed archive and returned as an `sf`
object with subnational geometries linked by GlobalDataLab codes.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file retrieval,
[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html) for
spatial data input

## Examples

``` r
if (FALSE) { # \dontrun{
shdi_shp <- read_shdi_shapefile()
} # }
```
