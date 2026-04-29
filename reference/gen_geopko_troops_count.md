# Generate Multi-Temporal Peacekeeping Troop Deployment Data

Creates a multi-layer raster containing the total number of UN
peacekeeping troops deployed within each PRIO-GRID cell across all
temporal intervals. The function aggregates Geo-PKO troop deployment
data to grid cells, providing a measure of peacekeeping force strength
and geographic distribution over time.

## Usage

``` r
gen_geopko_troops_count(config = pg_current_config())
```

## Value

A `SpatRaster` object

## Note

- Troop numbers marked as "unknown" in the source data are converted to
  NA and excluded

- Multiple deployments in the same cell are summed to give total troop
  presence

- Values represent troop numbers at deployment locations, not area-wide
  estimates

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
# Generate peacekeeping troop deployment data
pko_troops <- gen_geopko_troops_count()

# Examine the structure
print(pko_troops)

# View available time periods
time_periods <- names(pko_troops)
head(time_periods)

# Plot troop deployment for specific period
terra::plot(pko_troops[[1]],
            main = paste("Peacekeeping Troops:", time_periods[1]))

# Find periods with largest troop deployments
total_troops <- terra::global(pko_troops, "sum", na.rm = TRUE)
max_deployment_period <- which.max(total_troops$sum)
terra::plot(pko_troops[[max_deployment_period]],
            main = paste("Peak Deployment:", time_periods[max_deployment_period]))

# Analyze temporal trends in troop deployment
yearly_troop_totals <- terra::global(pko_troops, "sum", na.rm = TRUE)
plot(1:nlyr(pko_troops), yearly_troop_totals$sum,
     type = "l", main = "Total Peacekeeping Troops Over Time",
     xlab = "Time Period", ylab = "Total Troops Deployed")

# Identify major deployment locations
cumulative_troops <- terra::app(pko_troops, sum, na.rm = TRUE)
terra::plot(cumulative_troops,
            main = "Cumulative Peacekeeping Troop Deployments")

# Compare troop numbers vs. operation counts
pko_operations <- gen_geopko_operations_count()
# Compare specific periods
period_idx <- 10  # Example period
terra::plot(c(pko_operations[[period_idx]], pko_troops[[period_idx]]),
            main = c("Operations Count", "Troop Numbers"))

# Analyze troop concentration (troops per operation)
troops_per_op <- pko_troops / pko_operations
terra::plot(troops_per_op[[period_idx]],
            main = "Average Troops per Operation")
} # }
```
