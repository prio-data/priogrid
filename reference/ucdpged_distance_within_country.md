# Distance to nearest UCDP GED event within each country

For each PRIO-GRID cell, computes the distance (in meters) to the
nearest UCDP GED conflict event that occurred within the same country
during the period containing `measurement_date`. Cells in countries with
no recorded events are set to NA (not 0), to distinguish absence of
events from proximity to events. Events with spatial precision code \>=
6 (national-level or coarser) are excluded.

## Usage

``` r
ucdpged_distance_within_country(
  measurement_date,
  ged = read_ucdp_ged(),
  cshp = read_cshapes(),
  config = pg_current_config(),
  geodesic = NULL
)
```

## Arguments

- measurement_date:

  A single `Date` object specifying the measurement date.

- ged:

  An `sf` object containing UCDP GED data. Defaults to
  [`read_ucdp_ged()`](http://prio-data.github.io/priogrid/reference/read_ucdp_ged.md).

- cshp:

  An `sf` object containing CShapes 2.0 boundary data. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md).

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

- geodesic:

  Logical or NULL. If TRUE, computes distances in WGS84 using spherical
  (S2) geometry and reprojects the result to the config CRS. If FALSE,
  uses Euclidean distances in the config CRS. Default NULL auto-detects:
  geodesic for projected CRS (e.g. UTM), native for geographic CRS (e.g.
  WGS84, which terra already handles geodesically).

## Value

A `SpatRaster` with distances in meters to the nearest conflict event
within the same country. NA indicates no events recorded in that country
for the period.

## See also

[`ucdp_ged`](http://prio-data.github.io/priogrid/reference/ucdp_ged.md)
for fatality-count rasters,
[`read_ucdp_ged`](http://prio-data.github.io/priogrid/reference/read_ucdp_ged.md)
for loading raw UCDP GED data
