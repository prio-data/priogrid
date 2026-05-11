# Generate UCDP GED Battle-Related Deaths Variable

Creates a multi-layer raster containing aggregated fatality counts from
the UCDP Georeferenced Event Dataset for all PRIO-GRID temporal slices.
This is the standard generator function for including UCDP GED data in
PRIO-GRID.

## Usage

``` r
gen_ucdp_ged(config = pg_current_config())
```

## Value

A `SpatRaster` object with multiple layers, one for each time interval
defined in
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md).
Each layer contains:

- Cell values: Sum of best fatality estimates for events in that cell
  and period

- Layer names: Character representation of interval end dates

- NA values: Grid cells with no recorded events or outside state system

## Details

This function generates the standard UCDP GED variable for PRIO-GRID
using:

- The "best" fatality estimate from UCDP GED

- All three types of organized violence (state-based, non-state,
  one-sided)

- Temporal distribution of fatalities across PRIO-GRID intervals

- Spatial filtering to exclude events without sub-national precision

Events are filtered to include only those with spatial precision codes
1-5, which represent locations identified at sub-national level or
better. Events coded at national level (where_prec = 6) or with higher
uncertainty (where_prec = 7) are excluded. This filtering approach may
result in undercounting, particularly in regions where precise event
locations are difficult to determine.

The function uses the "best" fatality estimate, which represents UCDP's
assessment of the most likely number of fatalities based on available
sources. Alternative estimates (low, high) can be accessed through
[`ucdp_ged`](http://prio-data.github.io/priogrid/reference/ucdp_ged.md).

## Note

- Processing time depends on the size of the UCDP GED dataset

- The exclusion of imprecisely located events is a known limitation

- For custom configurations, use
  [`ucdp_ged`](http://prio-data.github.io/priogrid/reference/ucdp_ged.md)
  directly

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
for customizable processing options,
[`read_ucdp_ged`](http://prio-data.github.io/priogrid/reference/read_ucdp_ged.md)
for raw data access,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for temporal structure

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate standard UCDP GED variable
ged_fatalities <- gen_ucdp_ged()

# Examine the result
print(ged_fatalities)

# Check number of time periods
terra::nlyr(ged_fatalities)

# Plot first time period
terra::plot(ged_fatalities[[1]],
            main = paste("Fatalities:", names(ged_fatalities)[1]))

# Calculate annual totals
annual_total <- terra::app(ged_fatalities, sum, na.rm = TRUE)
terra::plot(annual_total, main = "Total Fatalities Across All Periods",
            col = hcl.colors(100, "YlOrRd", rev = TRUE))

# Identify hotspots
hotspots <- annual_total > 100
terra::plot(hotspots, main = "Conflict Hotspots (>100 fatalities)")
} # }
```
