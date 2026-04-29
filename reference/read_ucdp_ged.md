# Reads the UCDP Georeferenced Event Dataset (GED)

Downloads and processes the Uppsala Conflict Data Program's
Georeferenced Event Dataset (UCDP GED), which provides detailed
information on individual events of organized violence worldwide. The
function formats the data as a spatial object compatible with PRIO-GRID
temporal analysis.

## Usage

``` r
read_ucdp_ged()
```

## Value

A `sf` object containing georeferenced conflict events with:

- Geometry: Point locations of conflict events

- date_start, date_end: Event timing information

- date_interval: Lubridate interval for temporal operations

- best, low, high: Fatality estimates (best estimate, low bound, high
  bound)

- type_of_violence: Violence type classification (1, 2, or 3)

- where_prec: Spatial precision code (1-7)

- Coordinate reference system: WGS84 (EPSG:4326)

## Details

UCDP GED is the most disaggregated dataset on organized violence,
containing information on individual events including their location,
timing, actors, and fatality estimates. This function:

- Downloads UCDP GED version 25.1 if not already cached

- Converts the data to an sf spatial object with WGS84 coordinates

- Adds a date_interval utility column for temporal operations

- Preserves all original UCDP GED variables including fatality estimates

The dataset includes three types of organized violence:

- Type 1: State-based armed conflict

- Type 2: Non-state conflict

- Type 3: One-sided violence

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
for rasterizing events to PRIO-GRID, `ged_ucdp_ged` for generating the
standard UCDP GED variable

## Examples

``` r
if (FALSE) { # \dontrun{
# Read UCDP GED data
ged_data <- read_ucdp_ged()

# Examine the structure
print(ged_data)

# Check available columns
names(ged_data)

# Filter to state-based conflict only
state_conflict <- ged_data[ged_data$type_of_violence == 1, ]

# Plot event locations
plot(sf::st_geometry(ged_data), pch = ".",
     main = "UCDP GED Event Locations")

# Filter to specific time period
library(lubridate)
events_2020 <- ged_data[year(ged_data$date_start) == 2020, ]
} # }
```
