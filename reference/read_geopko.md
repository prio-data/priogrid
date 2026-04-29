# Reads the Geocoded Peacekeeping Operations (Geo-PKO) data

Downloads and processes Geo-PKO (Geocoded Peacekeeping Operations) data,
which provides spatial and temporal information on UN peacekeeping
deployments worldwide. The function formats coordinates as spatial
geometries and standardizes temporal variables for analysis.

## Usage

``` r
read_geopko()
```

## Value

A `sf` object

## Details

The dataset contains information on peacekeeping deployments including
mission details, troop numbers, deployment locations, and temporal
coverage. This enables analysis of peacekeeping presence, intensity, and
geographic distribution over time.

Geo-PKO tracks the locations and timing of UN peacekeeping operations at
high spatial and temporal resolution. This function:

- Downloads the Geo-PKO dataset from the data repository

- Filters out records with missing coordinate information

- Converts longitude/latitude coordinates to sf point geometries

- Sets coordinate reference system to WGS84 (EPSG:4326)

- Creates standardized date variables (mydate) from year-month
  information

- Rolls dates forward to the last day of each month for consistency

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
# Read Geo-PKO data
geopko_data <- read_geopko()

# Examine the structure
print(geopko_data)
str(geopko_data)

# Check available columns
names(geopko_data)

# View temporal coverage
range(geopko_data$mydate, na.rm = TRUE)

# Examine unique missions
if("mission" %in% names(geopko_data)) {
  unique_missions <- unique(geopko_data$mission)
  head(sort(unique_missions), 10)
}

# Plot peacekeeping deployment locations
plot(sf::st_geometry(geopko_data),
     main = "UN Peacekeeping Deployment Locations",
     pch = 16, cex = 0.5)

# Filter to specific time period
recent_deployments <- geopko_data[geopko_data$mydate >= as.Date("2010-01-01"), ]
plot(sf::st_geometry(recent_deployments),
     main = "Recent Peacekeeping Deployments (2010+)",
     pch = 16, cex = 0.5)

# Analyze deployment intensity by year
geopko_data$year_deployed <- lubridate::year(geopko_data$mydate)
yearly_deployments <- table(geopko_data$year_deployed)
plot(names(yearly_deployments), yearly_deployments,
     type = "l", main = "Peacekeeping Deployments Over Time",
     xlab = "Year", ylab = "Number of Deployments")

# Filter to specific mission (example)
# monusco_data <- geopko_data[grepl("MONUSCO", geopko_data$mission), ]
# plot(sf::st_geometry(monusco_data), main = "MONUSCO Deployments")
} # }
```
