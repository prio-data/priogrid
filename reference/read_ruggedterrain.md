# Read Global Multi-resolution Terrain Elevation Data (GMTED2010)

Reads the GMTED2010 spatial metadata for terrain elevation as an `sf`
object. This metadata includes polygon geometries for GMTED2010 tiles
and summary elevation statistics such as minimum, maximum, and mean
elevation values.

## Usage

``` r
read_ruggedterrain()
```

## Value

An `sf` object

## Details

The function performs the following steps:

1.  Locates the local GMTED2010 dataset file using
    [`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)

2.  Unzips the file to a local directory

3.  Reads the spatial metadata shapefile from the extracted data

## References

Danielson JJ, Gesch DB (2011). “Global Multi-resolution Terrain
Elevation Data 2010 (GMTED2010).” Technical Report 2011-1073, Earth
Resources Observation and Science (EROS) Center, Virginia.

## See also

[`ruggedterrain_variable`](http://prio-data.github.io/priogrid/reference/ruggedterrain_variable.md)
for calculating elevation statistics,
[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for the underlying data retrieval function

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the GMTED2010 elevation data
elevation_data <- read_ruggedterrain()
print(elevation_data)
} # }
```
