# Generate PRIO-GRID Compatible CRU Potential Evapotranspiration Data

This function processes CRU TS v4.09 potential evapotranspiration (PET)
data by aggregating it temporally and spatially to match PRIO-GRID
specifications.

## Usage

``` r
gen_cru_pet(config = pg_current_config())
```

## Value

A `SpatRaster` object with PRIO-GRID spatio-temporal resolution.

## See also

[`read_cru_pet`](http://prio-data.github.io/priogrid/reference/read_cru_pet.md),
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md),
[`robust_transformation`](http://prio-data.github.io/priogrid/reference/robust_transformation.md),
[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- gen_cru_pet()
} # }
```
