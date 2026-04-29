# Generate PRIO-GRID Compatible CRU Precipitation Data

This function processes CRU TS v4.09 precipitation (PRE) data by
aggregating it temporally and spatially to match PRIO-GRID
specifications.

## Usage

``` r
gen_cru_pre(config = pg_current_config())
```

## Value

A `SpatRaster` object with PRIO-GRID spatio-temporal resolution.

## See also

[`read_cru_pre`](http://prio-data.github.io/priogrid/reference/read_cru_pre.md),
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md),
[`robust_transformation`](http://prio-data.github.io/priogrid/reference/robust_transformation.md),
[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- gen_cru_pre()
} # }
```
