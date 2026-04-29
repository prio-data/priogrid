# Generate Multi-Temporal Regionally Excluded Ethnic Groups Coverage

Creates a multi-layer raster indicating PRIO-GRID cells that contain
settlement areas of regionally based ethnic groups experiencing
political exclusion. The function combines GeoEPR spatial data with EPR
Core political status information to map areas where excluded ethnic
groups are present over time.

## Usage

``` r
gen_geoepr_reg_excluded(
  excluded = c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION"),
  config = pg_current_config()
)
```

## Arguments

- excluded:

  A character vector specifying EPR status codes to classify as
  "excluded". Valid EPR status codes include:

  - "DISCRIMINATED": Groups facing systematic discrimination

  - "POWERLESS": Groups lacking political influence

  - "SELF-EXCLUSION": Groups voluntarily excluding themselves from
    politics

  - "MONOPOLY": Groups holding exclusive power

  - "DOMINANT": Groups in dominant power positions

  - "SENIOR PARTNER": Senior coalition partners

  - "JUNIOR PARTNER": Junior coalition partners

  - "IRRELEVANT": Groups without political relevance

  - "STATE COLLAPSE": Groups during state collapse periods

  Default is `c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION")`.

## Value

A `SpatRaster` object

## Details

The function focuses on "regionally based" ethnic groups, which have
identifiable settlement patterns that can be meaningfully mapped, as
opposed to groups that are dispersed throughout a country. Political
exclusion is defined by EPR status codes indicating systematic
discrimination, powerlessness, or voluntary self-exclusion from
political processes.

## References

Wucherpfennig J, Weidmann NB, Girardin L, Cederman L, Wimmer A (2011).
“Politically Relevant Ethnic Groups across Space and Time: Introducing
the GeoEPR Dataset.” *Conflict Management and Peace Science*, **28**(5),
423–437. ISSN 0738-8942, 1549-9219.
[doi:10.1177/0738894210393217](https://doi.org/10.1177/0738894210393217)
.
[2024-10-21](http://prio-data.github.io/priogrid/reference/2024-10-21).
Vogt M, Bormann N, Rüegger S, Cederman L, Hunziker P, Girardin K (2015).
“Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic
Power Relations Data Set Family.” *Journal of Conflict Resolution*,
**59**(7), 1327–42.
[2024-10-21](http://prio-data.github.io/priogrid/reference/2024-10-21).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate excluded groups coverage with default exclusion categories
excluded_coverage <- gen_geoepr_reg_excluded()

# Examine the structure
print(excluded_coverage)

# View available time periods
time_periods <- names(excluded_coverage)
head(time_periods)

# Plot coverage for specific period
terra::plot(excluded_coverage[[1]],
            main = "Excluded Ethnic Groups Coverage")

# Use custom exclusion definition (include only discriminated groups)
discriminated_only <- gen_geoepr_reg_excluded(excluded = "DISCRIMINATED")

# Compare different exclusion definitions
powerless_only <- gen_geoepr_reg_excluded(excluded = "POWERLESS")
combined_exclusion <- gen_geoepr_reg_excluded(excluded = c("DISCRIMINATED", "POWERLESS"))

# Analyze temporal changes in exclusion
first_period <- excluded_coverage[[1]]
last_period <- excluded_coverage[[nlyr(excluded_coverage)]]
exclusion_change <- last_period - first_period
terra::plot(exclusion_change, main = "Change in Excluded Group Presence")

# Calculate total excluded area over time
total_excluded_cells <- terra::global(excluded_coverage > 0, "sum", na.rm = TRUE)
plot(total_excluded_cells$sum, type = "l",
     main = "Number of Cells with Excluded Groups Over Time")
} # }
```
