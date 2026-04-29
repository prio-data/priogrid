# Generate SPEI-6 with mean-temporal aggregation compatible with PRIO-GRID

The function takes monthly SPEIbase data and transforms it to the
temporal resolution defined by PRIO-GRID date intervals (which may be
quarterly, yearly, or other intervals), while also performing spatial
aggregation to the PRIO-GRID resolution.

## Usage

``` r
gen_speibase6_mean(config = pg_current_config())
```

## Value

A `SpatRaster` object (from the terra package) with spatio-temporal
resolution as defined in PRIO-GRID.

## References

Beguería S, Latorre B, Reig F, Vicente-Serrano SM (2024). “SPEIbase
v.2.11.” Beguería S, Vicente-Serrano SM, Reig F, Latorre B (2014).
“Standardized Precipitation Evapotranspiration Index (SPEI) Revisited:
Parameter Fitting, Evapotranspiration Models, Tools, Datasets and
Drought Monitoring.” *International Journal of Climatology*, **34**(10),
3001–3023. ISSN 1097-0088.
[doi:10.1002/joc.3887](https://doi.org/10.1002/joc.3887) .
[2018-08-29](http://prio-data.github.io/priogrid/reference/2018-08-29).

## See also

[`read_speibase`](http://prio-data.github.io/priogrid/reference/read_speibase.md)
for reading and calculating SPEIbase,
[`read_cru_pet`](http://prio-data.github.io/priogrid/reference/read_cru_pet.md)
for input-data to SPEIbase,
[`read_cru_pre`](http://prio-data.github.io/priogrid/reference/read_cru_pre.md)
for input-data to SPEIbase,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for PRIO-GRID temporal boundaries,
[`robust_transformation`](http://prio-data.github.io/priogrid/reference/robust_transformation.md)
for spatial aggregation,
[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file management

## Examples

``` r
if (FALSE) { # \dontrun{
# SPEI-6 averaged over PRIO-GRID time-interval using the mean.
spei6_mean <- gen_speibase6_mean()
} # }
```
