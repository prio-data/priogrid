# Reads in SHDI data and fixes empty SHDI geometries

This function processes GlobalDataLab Subnational Human Development
Index (SHDI) data, resolving missing or empty geometries using
geoBoundaries crosswalks, and returns a sf data.frame

## Usage

``` r
read_shdi(
  shdi_csv = read_shdi_csv(),
  shp = read_shdi_shapefile(),
  geoboundaries = read_geoboundaries(),
  fix_empty = TRUE
)
```

## Arguments

- shdi_csv:

  A data frame containing SHDI data as returned by
  [`read_shdi_csv`](http://prio-data.github.io/priogrid/reference/read_shdi_csv.md).

- shp:

  An `sf` object containing SHDI geometries as returned by
  [`read_shdi_shapefile`](http://prio-data.github.io/priogrid/reference/read_shdi_shapefile.md).

- geoboundaries:

  An `sf` object containing administrative boundary geometries as
  returned by
  [`read_geoboundaries`](http://prio-data.github.io/priogrid/reference/read_geoboundaries.md).

- fix_empty:

  A boolean whether or not to fix SHDI empty geometries. Defaults to
  TRUE.

## Value

An `sf` object containing SHDI data and geometries.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`read_shdi_csv`](http://prio-data.github.io/priogrid/reference/read_shdi_csv.md),
[`read_shdi_shapefile`](http://prio-data.github.io/priogrid/reference/read_shdi_shapefile.md),
[`read_geoboundaries`](http://prio-data.github.io/priogrid/reference/read_geoboundaries.md),
[`prio_blank_grid`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate PRIO-GRID SHDI
shdi <- read_shdi()
} # }
```
