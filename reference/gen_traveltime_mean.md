# Generate the mean travel time to nearest major city

Aggregates the high-resolution travel time raster to PRIO-GRID cells and
computes the **mean** travel time within each cell. This is useful for
identifying the average travel access to major cities within a PRIO-GRID
cell.

## Usage

``` r
gen_traveltime_mean(config = pg_current_config())
```

## Value

A single-layer `SpatRaster` object

## Details

This function is a convenience wrapper around
[`calc_traveltime`](http://prio-data.github.io/priogrid/reference/calc_traveltime.md)
using `aggregation_function = "mean"`. It reads the global travel time
raster and summarizes each PRIO-GRID cell to the mean travel time value.

## References

Nelson (2008). “Travel Time to Major Cities: A Global Map of
Accessibility.”
[2025-05-14](http://prio-data.github.io/priogrid/reference/2025-05-14).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate mean travel time raster
r <- gen_traveltime_mean()
terra::plot(r, main = "Mean Travel Time to Nearest Major City")

# Extract values for a specific region (e.g., West Africa)
africa_extent <- terra::ext(-20, 20, 0, 20)
r_africa <- terra::crop(r, africa_extent)
terra::plot(r_africa, main = "Mean Travel Time in West Africa")
} # }
```
