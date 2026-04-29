# Reads the CShapes 2.0 raw data

Downloads and processes CShapes 2.0 historical country boundaries data.
The function formats date columns and adds utility columns for temporal
analysis compatible with PRIO-GRID.

## Usage

``` r
read_cshapes()
```

## Value

A `sf` object containing historical country boundaries with:

- Geometry: Polygon/multipolygon country boundaries

- gwsdate, gwedate: Properly formatted Date objects for country periods

- date_interval: Utility column for temporal analysis

- Coordinate reference system: WGS84 (EPSG:4326)

## Details

CShapes 2.0 provides historical country boundary data from 1886 onwards
at high spatial resolution. This function:

- Downloads the CShapes 2.0 dataset if not already cached

- Converts gwsdate and gwedate columns to proper Date objects

- Adds a date_interval utility column for temporal operations

- Returns data in sf format for spatial operations

## References

Schvitz G, Girardin L, Rüegger S, Weidmann NB, Cederman L, Gleditsch KS
(2022). “Mapping the International System, 1886-2019: The CShapes 2.0
Dataset.” *Journal of Conflict Resolution*, **66**(1), 144–161. ISSN
0022-0027.
[doi:10.1177/00220027211013563](https://doi.org/10.1177/00220027211013563)
.
[2024-11-22](http://prio-data.github.io/priogrid/reference/2024-11-22).

## See also

[`pg_dates`](http://prio-data.github.io/priogrid/reference/pg_dates.md)
for PRIO-GRID temporal coverage,
[`st_sf`](https://r-spatial.github.io/sf/reference/sf.html) for sf
object details

## Examples

``` r
if (FALSE) { # \dontrun{
# Read CShapes 2.0 data
cshapes_data <- read_cshapes()

# Examine the structure
print(cshapes_data)

# Check date formatting
head(cshapes_data$gwsdate)
head(cshapes_data$gwedate)

# View available columns
names(cshapes_data)

# Filter to specific time period
modern_boundaries <- cshapes_data[cshapes_data$gwsdate >= as.Date("2000-01-01"), ]

# Plot boundaries for a specific period
plot(sf::st_geometry(modern_boundaries), main = "Country Boundaries (2000+)")
} # }
```
