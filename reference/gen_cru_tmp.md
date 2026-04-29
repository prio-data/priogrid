# Generate PRIO-GRID Compatible CRU Near Surface Temperature Data

This function processes CRU TS (Climate Research Unit Time Series) v4.09
near surface temperature data by aggregating it both temporally and
spatially to match PRIO-GRID specifications. The function takes monthly
CRU temperature data and transforms it to the temporal resolution
defined by PRIO-GRID date intervals (which may be quarterly, yearly, or
other intervals), while also performing spatial aggregation to the
PRIO-GRID resolution.

## Usage

``` r
gen_cru_tmp(config = pg_current_config())
```

## Value

A `SpatRaster` object (from the terra package) with spatio-temporal
resolution as defined in PRIO-GRID.

## Details

This takes the CRU TS dataset and aggregates it in time and space to
PRIO-GRID specifications.

## References

Harris I, Osborn TJ, Jones P, Lister D (2020). “Version 4 of the CRU TS
Monthly High-Resolution Gridded Multivariate Climate Dataset.”
*Scientific Data*, **7**(1), 109. ISSN 2052-4463.
[doi:10.1038/s41597-020-0453-3](https://doi.org/10.1038/s41597-020-0453-3)
.

## See also

[`read_cru_tmp`](http://prio-data.github.io/priogrid/reference/read_cru_tmp.md)
for reading raw CRU temperature data,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for PRIO-GRID temporal boundaries,
[`robust_transformation`](http://prio-data.github.io/priogrid/reference/robust_transformation.md)
for spatial aggregation,
[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file management

## Examples

``` r
if (FALSE) { # \dontrun{
r <- gen_cru_tmp()
} # }
```
