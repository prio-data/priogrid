# Reads the HILDA+ data

Downloads and processes HILDA+ (Historic Land Dynamics Assessment)
global land use and land cover change data, filtering to years
compatible with PRIO-GRID temporal coverage. The function returns a
multi-layer raster with annual land use/land cover states.

## Usage

``` r
read_hilda(config = pg_current_config())
```

## Value

A `SpatRaster` object (terra package) containing annual land use/land
cover states. Each layer represents one year, with layer names formatted
as dates (YYYY-MM-DD). The raster has:

- Spatial resolution: 0.01 degrees (approximately 1 km)

- Coordinate reference system: WGS84 (EPSG:4326)

- Global extent: -180 to 180° longitude, -90 to 90° latitude

- Temporal coverage: Years matching PRIO-GRID dates (max 1899-2019)

The function depends on the internal
[get_pgfile](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
and
[pg_dates](http://prio-data.github.io/priogrid/reference/pg_dates.md)
functions from the priogrid package.

an object of class sf

## Details

HILDA+ provides global land use and land cover change data at 1 km
spatial resolution from 1960 to 2019. This function:

- Extracts downloaded raster files containing annual land use/land cover
  states

- Filters years to match PRIO-GRID temporal coverage

- Creates a multi-layer SpatRaster with standardized date names

The land use/land cover classes in HILDA+ include various categories
such as: cropland, pasture, urban areas, forests, and other natural land
covers. Each pixel value represents a specific land use/land cover class
according to the HILDA+ classification scheme.

## References

Winkler K, Fuchs R, Rounsevell MDA, Herold M (2020). “HILDA+ Global Land
Use Change between 1960 and 2019.”
[doi:10.1594/PANGAEA.921846](https://doi.org/10.1594/PANGAEA.921846) .
[2025-05-26](http://prio-data.github.io/priogrid/reference/2025-05-26).

## See also

[`pg_dates`](http://prio-data.github.io/priogrid/reference/pg_dates.md)
for PRIO-GRID temporal coverage,
[`rast`](https://rspatial.github.io/terra/reference/rast.html) for
SpatRaster object details

## Examples

``` r
if (FALSE) { # \dontrun{
# Read HILDA+ data
hilda_data <- read_hilda()

# Examine the structure
print(hilda_data)

# Get available years
years <- names(hilda_data)
print(years)

# Extract data for a specific year (e.g., 2000)
hilda_2000 <- hilda_data[["2000-12-31"]]

# Plot land use for a specific year
terra::plot(hilda_2000, main = "HILDA+ Land Use 2000")
} # }
```
