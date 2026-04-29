# Process UCDP GED Data with Temporal Fatality Distribution

Processes UCDP GED conflict data by distributing fatalities across
PRIO-GRID temporal intervals and rasterizing the results. Events
spanning multiple time periods have their fatalities proportionally
allocated based on temporal overlap, ensuring accurate representation in
panel data structures.

## Usage

``` r
ucdp_ged(
  ged = read_ucdp_ged(),
  violence_types = c(1, 2, 3),
  fatality_variable = "best",
  config = pg_current_config()
)
```

## Arguments

- ged:

  An `sf` object containing UCDP GED data. Defaults to
  [`read_ucdp_ged()`](http://prio-data.github.io/priogrid/reference/read_ucdp_ged.md)
  if not provided.

- violence_types:

  Integer vector specifying which types of violence to include. Options
  are:

  - 1: State-based armed conflict

  - 2: Non-state conflict

  - 3: One-sided violence

  Default is `c(1, 2, 3)` (all types).

- fatality_variable:

  Character string specifying which fatality estimate to use. Options
  are:

  - "best": Best estimate of fatalities (default)

  - "low": Low estimate of fatalities

  - "high": High estimate of fatalities

  - "event_count": Count of events rather than fatalities

## Value

A `SpatRaster` object with multiple layers, one for each time interval
in
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md).
Layer names correspond to interval end dates. Cell values represent
aggregated fatalities (or event counts) for each grid cell and time
period.

## Details

This function implements a sophisticated fatality distribution algorithm
that:

- Calculates the temporal overlap between each event and PRIO-GRID
  intervals

- Distributes fatalities proportionally based on the number of
  overlapping days

- Uses integer allocation with remainder distribution to preserve total
  counts

- Handles edge cases where events span multiple periods or have single
  fatalities

- Validates that distributed fatalities sum to original event totals

The distribution algorithm ensures that:

- Total fatalities are preserved (no over- or under-counting)

- Single-fatality events are assigned to exactly one period

- Fractional allocations are resolved using largest remainder method

Events without sub-national spatial precision (where_prec \>= 6) are
excluded from the final rasterization, which may result in undercounting
in some regions.

## Note

- The function requires the data.table package for efficient processing

- Processing time scales with the number of events and time intervals

- A warning is issued if fatality distribution validation fails

- Events coded at national level or coarser precision are excluded

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

[`read_ucdp_ged`](http://prio-data.github.io/priogrid/reference/read_ucdp_ged.md)
for loading raw UCDP GED data, `ged_ucdp_ged` for the standard generator
function,
[`pg_date_intervals`](http://prio-data.github.io/priogrid/reference/pg_date_intervals.md)
for PRIO-GRID temporal structure

## Examples

``` r
if (FALSE) { # \dontrun{
# Process all violence types with best fatality estimates
ged_raster <- ucdp_ged()

# Examine the result
print(ged_raster)

# Process state-based conflict only
state_conflict <- ucdp_ged(violence_types = 1)

# Use high fatality estimates
ged_high <- ucdp_ged(fatality_variable = "high")

# Count events instead of fatalities
event_counts <- ucdp_ged(fatality_variable = "event_count")

# Plot a single time slice
terra::plot(ged_raster[[1]],
            main = paste("UCDP GED Fatalities:", names(ged_raster)[1]))

# Calculate total fatalities across all periods
total_fatalities <- terra::app(ged_raster, sum, na.rm = TRUE)
terra::plot(total_fatalities, main = "Total Fatalities (All Periods)")
} # }
```
