# Make SPEIbase data PRIO-GRID compatible

The function takes monthly SPEIbase data and transforms it to the
temporal resolution defined by PRIO-GRID date intervals (which may be
quarterly, yearly, or other intervals), while also performing spatial
aggregation to the PRIO-GRID resolution.

## Usage

``` r
speibaseN(interval, time_agg_fun, config = pg_current_config())
```

## Arguments

- interval:

  Integer. The month interval to calculate SPEI over. E.g., if the
  interval is 6, then monthly precipitation and potential
  evapotranspiration are aggregated over the last 6 months, then the
  SPEI (anomaly) is calculated. This is done for each month.

- time_agg_fun:

  Character. Either "mean" or "max". Original data is monthly, so if
  PRIO-GRID is lower resolution, then we need to aggregate over time.
  Currently, mean and max functions are implemented.

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
spei6_mean <- speibaseN(interval = 6, time_agg_fun = "mean")
} # }
```
