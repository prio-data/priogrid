# Generate multi-temporal distance to international borders data (bdist2)

Creates a multi-layer raster containing distances to nearest
international borders for all PRIO-GRID temporal slices. The function
generates time series data showing how proximity to international
boundaries changes over time due to territorial changes, state
formation, and dissolution events.

## Usage

``` r
gen_bdist2(cshp = read_cshapes(), config = pg_current_config())
```

## Arguments

- cshp:

  An `sf` object containing CShapes 2.0 boundary data with temporal
  information. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  if not provided.

## Value

A `SpatRaster` object

## Details

The function automatically uses past results from previous time slices
to dramatically reduce computation time when country boundaries remain
unchanged between consecutive periods. This makes temporal sequence
generation much more efficient than calculating each period
independently.

## Note

- This function is computationally intensive and may take hours to
  complete

- Progress indicators are printed during processing (time slice numbers)

- The optimization using past results significantly reduces total
  computation time

- Consider running in segments for very long time series to manage
  memory usage

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
# Generate full temporal border distance dataset
# Warning: This may take several hours to complete
temporal_bdist2 <- gen_bdist2()

print(temporal_bdist2)
} # }
```
