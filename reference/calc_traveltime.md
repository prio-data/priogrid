# Generate travel time to nearest major city

Aggregates the high-resolution global travel time raster to the
PRIO-GRID structure. Users can specify the aggregation function to
summarize travel time values within each PRIO-GRID cell.

## Usage

``` r
calc_traveltime(aggregation_function, config = pg_current_config())
```

## Arguments

- aggregation_function:

  Function or character string specifying the aggregation method (see
  details above)

## Value

A `SpatRaster` object

## Details

Supported aggregation functions include: `"mean"`, `"max"`, `"min"`,
`"median"`, `"sum"`, `"modal"`, `"any"`, `"all"`, `"prod"`,
`"which.min"`, `"which.max"`, `"table"`, `"sd"` (sample standard
deviation), and `"std"` (population standard deviation). Users may also
provide a custom function.

## References

Nelson (2008). “Travel Time to Major Cities: A Global Map of
Accessibility.”
[2025-05-14](http://prio-data.github.io/priogrid/reference/2025-05-14).

## Examples

``` r
if (FALSE) { # \dontrun{
# Aggregate global travel time using median
travel_time_median <- calc_traveltime(aggregation_function = "median")
terra::plot(travel_time_median, main = "PRIO-GRID Median Travel Time")

# Aggregate using maximum travel time
travel_time_max <- calc_traveltime(aggregation_function = "max")
terra::plot(travel_time_max, main = "PRIO-GRID Maximum Travel Time")
} # }
```
