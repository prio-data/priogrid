# Reads the GeoEPR - Geo-referencing Ethnic Power Relations data

Downloads and processes GeoEPR (Geo-referencing Ethnic Power Relations)
data, which provides spatial information on politically relevant ethnic
groups and their settlement areas worldwide. The function formats
temporal variables and adds utility columns for temporal analysis
compatible with PRIO-GRID.

## Usage

``` r
read_geoepr()
```

## Value

A `sf` object

## Details

GeoEPR combines the Ethnic Power Relations (EPR) dataset with geographic
information to map politically relevant ethnic groups' settlement
patterns. This function:

- Downloads the GeoEPR dataset from the ETH ICR data repository

- Converts temporal variables (from, to) to proper Date objects
  (gwsdate, gwedate)

- Adds a date_interval utility column for temporal operations

- Filters out geometries with empty spatial information

- Returns data in sf format for spatial analysis

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

## See also

[`pg_dates`](http://prio-data.github.io/priogrid/reference/pg_dates.md)
for PRIO-GRID temporal coverage,
[`st_sf`](https://r-spatial.github.io/sf/reference/sf.html) for sf
object details

## Examples

``` r
if (FALSE) { # \dontrun{
# Read GeoEPR data
geoepr_data <- read_geoepr()

print(geoepr_data)

# Check available columns
names(geoepr_data)

# View temporal coverage
summary(geoepr_data$gwsdate)
summary(geoepr_data$gwedate)

# Filter to specific time period
modern_groups <- geoepr_data[geoepr_data$gwsdate >= as.Date("2000-01-01"), ]

# Examine unique ethnic groups
unique_groups <- unique(geoepr_data$group)
head(unique_groups, 10)

# Filter to specific country (example: using country code)
# country_groups <- geoepr_data[geoepr_data$gwid == 2, ]  # Example for USA

# Plot ethnic group settlement areas
plot(sf::st_geometry(geoepr_data), main = "Ethnic Group Settlement Areas")
} # }
```
