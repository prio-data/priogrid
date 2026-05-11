# Extract contiguous urban extent around a location

Identifies and delineates the contiguous urban extent surrounding a
specified geographic location using GHS-WUP-DEGURBA (Degree of
Urbanisation) data. The function extracts the urban patch that contains
the input coordinates, providing a polygon boundary of the connected
urban area.

## Usage

``` r
urban_extent(
  lon,
  lat,
  measurement_date,
  urban_definition = c(21, 22, 23, 30),
  max_extent = 1e+06,
  config = pg_current_config()
)
```

## Arguments

- lon:

  Numeric. Longitude of the location of interest (WGS84, decimal
  degrees).

- lat:

  Numeric. Latitude of the location of interest (WGS84, decimal
  degrees).

- measurement_date:

  Character. Date for which to extract urban extent, in format
  "YYYY-MM-DD" (e.g., "2020-12-31"). Must match one of the available
  dates in the GHS-WUP-DEGURBA dataset (typically at 5-year intervals:
  1975, 1980, ..., 2030).

- urban_definition:

  Numeric vector of DEGURBA codes to classify as urban. Default is
  `c(21, 22, 23, 30)` (suburban, semi-dense, dense, and urban centres).
  Valid codes are: 10, 11, 12, 13, 21, 22, 23, 30. See Details for code
  meanings.

- max_extent:

  Numeric. Maximum search radius in meters around the input coordinates.
  Default is 1000000 (1000 km). This limits the area searched for
  connected urban patches and improves computational efficiency.

## Value

An `sf` polygon object representing the contiguous urban extent
containing the input location. The polygon is returned in the coordinate
reference system specified by the current config CRS. If the input
location is not classified as urban, the function may return an empty
polygon or fail.

## Details

The function performs the following workflow:

1.  Creates a circular buffer of radius `max_extent` around the input
    coordinates

2.  Extracts GHS-WUP-DEGURBA data for the specified `measurement_date`

3.  Crops the raster to the buffer area for computational efficiency

4.  Reclassifies cells to binary urban (1) or non-urban (0) based on
    `urban_definition`

5.  Identifies all contiguous urban patches using 8-directional
    connectivity

6.  Determines which patch contains the input coordinates

7.  Extracts and dissolves the contiguous urban patch into a single
    polygon

The DEGURBA classification codes represent:

- 30: Urban centres (cities)

- 23: Dense urban cluster

- 22: Semi-dense urban cluster

- 21: Suburban or peri-urban

- 13: Rural cluster

- 12: Low density rural

- 11: Very low density rural

- 10: Water bodies or uninhabited areas

**Contiguity**: Urban patches are defined using 8-directional
connectivity, meaning cells are considered connected if they share an
edge or corner.

## Note

- The function requires the input coordinates to fall within an urban
  area

- Larger `max_extent` values increase computational time and memory
  usage

- The urban definition significantly affects the resulting extent

- Stricter definitions (e.g., only code 30) produce smaller extents

- Broader definitions (e.g., codes 21-30) produce larger extents

- Processing time varies with the size of the urban area

## References

Schiavina M, Melchiorri M, Pesaresi M, Jacobs-Crisioni C, Dijkstra L
(2025). “GHS-WUP-DEGURBA R2025A – GHS-WUP DEGURBA Settlement Layers,
Application of the Degree of Urbanisation Methodology (Stage I) to
GHS-WUP-POP R2025A, Multitemporal (1975-2100).”
[doi:10.2905/1c049178-ab00-4bbc-b638-3e3c19daaacb](https://doi.org/10.2905/1c049178-ab00-4bbc-b638-3e3c19daaacb)
.

European Commission, Statistical Office of the European Union (2021).
“Applying the Degree of Urbanisation — A Methodological Manual to Define
Cities, Towns and Rural Areas for International Comparisons.”
[doi:10.2785/706535](https://doi.org/10.2785/706535) .

## See also

[`read_ghs_wup_degurba`](http://prio-data.github.io/priogrid/reference/read_ghs_wup_degurba.md)
for reading the underlying DEGURBA data

[`ghs_wup_degurba`](http://prio-data.github.io/priogrid/reference/ghs_wup_degurba.md)
for custom urban classification

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract urban extent for Oslo, Norway (2020)
oslo_extent <- urban_extent(
  lon = 10.763063,
  lat = 59.935320,
  measurement_date = "2020-12-31"
)

# Plot the result
plot(sf::st_geometry(oslo_extent), main = "Oslo Urban Extent 2020")

# Extract urban extent for New Delhi, India (2020)
delhi_extent <- urban_extent(
  lon = 77.231487,
  lat = 28.612738,
  measurement_date = "2020-12-31"
)

# Compare urban extent over time for the same location
paris_1975 <- urban_extent(
  lon = 2.3522,
  lat = 48.8566,
  measurement_date = "1975-12-31"
)
paris_2020 <- urban_extent(
  lon = 2.3522,
  lat = 48.8566,
  measurement_date = "2020-12-31"
)

# Calculate urban area expansion
area_1975 <- sf::st_area(paris_1975)
area_2020 <- sf::st_area(paris_2020)
expansion <- (area_2020 - area_1975) / area_1975 * 100
cat("Urban expansion:", round(expansion, 1), "%\n")

# Use stricter urban definition (only urban centres)
tokyo_core <- urban_extent(
  lon = 139.6917,
  lat = 35.6895,
  measurement_date = "2020-12-31",
  urban_definition = 30
)

# Use broader urban definition (including all urbanized areas)
tokyo_metro <- urban_extent(
  lon = 139.6917,
  lat = 35.6895,
  measurement_date = "2020-12-31",
  urban_definition = c(21, 22, 23, 30)
)

# Reduce search radius for smaller cities
kongsvinger <- urban_extent(
  lon = 12.000669,
  lat = 60.190700,
  measurement_date = "2020-12-31",
  max_extent = 50e3  # 50 km radius
)

# Extract multiple cities and compare
cities <- data.frame(
  name = c("London", "Paris", "Berlin"),
  lon = c(-0.1276, 2.3522, 13.4050),
  lat = c(51.5074, 48.8566, 52.5200)
)

city_extents <- lapply(1:nrow(cities), function(i) {
  urban_extent(
    lon = cities$lon[i],
    lat = cities$lat[i],
    measurement_date = "2020-12-31"
  )
})

# Calculate areas
city_areas <- sapply(city_extents, sf::st_area)
cities$area_km2 <- as.numeric(city_areas) / 1e6
print(cities)
} # }
```
