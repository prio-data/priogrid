# Reads the Natural Earth 10m Physical Land Data

Downloads and imports the Natural Earth 1:10m scale Physical Land
dataset, which provides global polygon boundaries of landmasses at a
coarse cartographic scale suitable for mapping and contextual analysis.

## Usage

``` r
read_naturalearth_10m_land()
```

## Value

An `sf` object

## Details

The function:

- Downloads the Natural Earth 1:10m Physical Land shapefile from the
  PRIO-GRID data repository

- Extracts the contents of the zip archive

- Reads the shapefile into R as an `sf` object

## Note

- The Natural Earth dataset is intended for mapping and general
  analysis, not for precise cadastral or legal boundary use

- The file is cached locally after download

## References

Natural Earth (2024). “Land. 10m Physical. Made with Natural Earth. Free
Vector and Raster Map Data.”

## Examples

``` r
if (FALSE) { # \dontrun{
# Read Natural Earth 10m land polygons
land <- read_naturalearth_10m_land()

# Inspect structure
print(land)

# Plot global landmasses
plot(sf::st_geometry(land), col = "lightgray", border = "darkgray")
} # }
```
