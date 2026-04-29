# Generate cshapes_gwcode variable

Creates a multi-layer raster containing Gleditsch-Ward country codes for
all PRIO-GRID temporal slices. The function generates time series data
showing how country assignments to grid cells change over time due to
territorial changes, state formation, and dissolution events.

## Usage

``` r
gen_cshapes_gwcode(cshp = read_cshapes(), config = pg_current_config())
```

## Arguments

- cshp:

  An `sf` object containing CShapes 2.0 boundary data with
  Gleditsch-Ward country codes and temporal information. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  if not provided.

## Value

A `SpatRaster` object

## References

Schvitz G, Girardin L, Rüegger S, Weidmann NB, Cederman L, Gleditsch KS
(2022). “Mapping the International System, 1886-2019: The CShapes 2.0
Dataset.” *Journal of Conflict Resolution*, **66**(1), 144–161. ISSN
0022-0027.
[doi:10.1177/00220027211013563](https://doi.org/10.1177/00220027211013563)
.
[2024-11-22](http://prio-data.github.io/priogrid/reference/2024-11-22).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate full temporal country code dataset
temporal_gwcodes <- gen_cshapes_gwcode()

print(temporal_gwcodes)
} # }
```
