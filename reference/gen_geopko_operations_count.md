# Generate Multi-Temporal Peacekeeping Operations Count Data

Creates a multi-layer raster containing counts of UN peacekeeping
operations within each PRIO-GRID cell across all temporal intervals. The
function aggregates Geo-PKO deployment locations to grid cells,
providing a measure of peacekeeping intensity and geographic
distribution over time.

## Usage

``` r
gen_geopko_operations_count(config = pg_current_config())
```

## Value

A `SpatRaster` object

## References

Cil D, Fjelde H, Hultman L, Nilsson D (2020). “Mapping Blue Helmets:
Introducing the Geocoded Peacekeeping Operations (Geo-PKO) Dataset.”
*Journal of Peace Research*, **57**(2), 360–370. ISSN 0022-3433,
1460-3578.
[doi:10.1177/0022343319871978](https://doi.org/10.1177/0022343319871978)
.
[2024-10-21](http://prio-data.github.io/priogrid/reference/2024-10-21).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate peacekeeping operations count data
pko_counts <- gen_geopko_operations_count()

# Examine the structure
print(pko_counts)

# View available time periods
time_periods <- names(pko_counts)
head(time_periods)
tail(time_periods)

# Plot operations count for specific period
terra::plot(pko_counts[[1]],
            main = paste("Peacekeeping Operations Count:", time_periods[1]))

# Find periods with highest peacekeeping activity
total_operations <- terra::global(pko_counts, "sum", na.rm = TRUE)
max_activity_period <- which.max(total_operations$sum)
terra::plot(pko_counts[[max_activity_period]],
            main = paste("Peak Activity Period:", time_periods[max_activity_period]))

# Analyze temporal trends in peacekeeping deployment
yearly_totals <- terra::global(pko_counts, "sum", na.rm = TRUE)
plot(1:nlyr(pko_counts), yearly_totals$sum,
     type = "l", main = "Total Peacekeeping Operations Over Time",
     xlab = "Time Period", ylab = "Total Operations Count")

# Identify hotspots of peacekeeping activity
total_pko_activity <- terra::app(pko_counts, sum, na.rm = TRUE)
terra::plot(total_pko_activity,
            main = "Cumulative Peacekeeping Operations")

# Compare early vs. recent periods
early_period <- pko_counts[[1:5]]  # First 5 periods
recent_period <- pko_counts[[(nlyr(pko_counts)-4):nlyr(pko_counts)]]  # Last 5 periods
early_total <- terra::app(early_period, sum, na.rm = TRUE)
recent_total <- terra::app(recent_period, sum, na.rm = TRUE)
terra::plot(c(early_total, recent_total),
            main = c("Early Period PKO", "Recent Period PKO"))
} # }
```
