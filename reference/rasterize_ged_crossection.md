# Rasterize UCDP GED Events for a Single Time Interval

Internal function that aggregates UCDP GED conflict events to the
PRIO-GRID raster structure for a specified time interval. The function
filters events by temporal overlap and spatial precision before
rasterization.

## Usage

``` r
rasterize_ged_crossection(
  ged,
  pg_interval,
  fatality_variable,
  config = pg_current_config()
)
```

## Arguments

- ged:

  An `sf` object containing UCDP GED data, typically from
  [`read_ucdp_ged`](http://prio-data.github.io/priogrid/reference/read_ucdp_ged.md)
  or processed through
  [`ucdp_ged`](http://prio-data.github.io/priogrid/reference/ucdp_ged.md).

- pg_interval:

  A `lubridate` interval object specifying the time period for
  aggregation.

- fatality_variable:

  Character string specifying which fatality estimate to aggregate.
  Options include "best", "low", "high", or "event_count" for counting
  events rather than fatalities.

## Value

A `SpatRaster` object with aggregated values for the specified time
interval. Layer name is set to the interval end date.

## Details

This function performs the following operations:

- Transforms event coordinates to match PRIO-GRID projection

- Filters events to those overlapping the specified time interval

- Removes events with spatial precision code \>= 6 (sub-national
  precision required)

- Filters by specified violence types

- Rasterizes events by summing the specified fatality variable or
  counting events

Spatial precision filtering removes events coded at national level or
higher uncertainty, which may lead to undercounting in some regions.

## References

Sundberg R, Melander E (2013). “Introducing the UCDP Georeferenced Event
Dataset.” *Journal of Peace Research*, **50**(4), 523–532. ISSN
0022-3433, 1460-3578.
[doi:10.1177/0022343313484347](https://doi.org/10.1177/0022343313484347)
.
[2024-10-21](http://prio-data.github.io/priogrid/reference/2024-10-21).

Davies S, Pettersson T, Sollenberg M, Öberg M (2025). “Organized
Violence 1989–2024, and the Challenges of Identifying Civilian Victims.”
*Journal of Peace Research*, **62**(4), 1223–1240. ISSN 0022-3433.
[doi:10.1177/00223433251345636](https://doi.org/10.1177/00223433251345636)
.
[2025-06-11](http://prio-data.github.io/priogrid/reference/2025-06-11).

## See also

[`ucdp_ged`](http://prio-data.github.io/priogrid/reference/ucdp_ged.md)
for the main processing function,
[`prio_blank_grid`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)
for PRIO-GRID structure
