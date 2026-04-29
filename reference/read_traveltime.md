# Read Estimated Travel Time to Major Cities (Year 2000)

Downloads and reads the global raster dataset of estimated travel time
to major cities for the year 2000, as developed by Nelson (2008). The
dataset represents travel time in minutes to the nearest major city
using multimodal transport networks.

## Usage

``` r
read_traveltime()
```

## Value

A `SpatRaster` object

## Details

The function:

- Locates the local travel time dataset using
  [`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)

- Extracts the zip archive containing the raster files

- Imports the main `.tif` file as a `SpatRaster` object using the
  `terra` package

- Sets the layer name to `"travel_time"` for clarity

## Note

- The dataset provides estimates based on the transportation network
  circa 2000

- Raster extraction is cached locally after the first download

- Large raster files may take time and memory to process

## References

Nelson (2008). “Travel Time to Major Cities: A Global Map of
Accessibility.”
[2025-05-14](http://prio-data.github.io/priogrid/reference/2025-05-14).

## Examples

``` r
if (FALSE) { # \dontrun{
# Load estimated travel time raster
travel_time_raster <- read_traveltime()

# Inspect raster properties
travel_time_raster

# Plot global travel time
terra::plot(travel_time_raster, main = "Estimated Travel Time to Major Cities (2000)")

# Extract travel time for a specific region
africa_extent <- terra::ext(-20, 50, -35, 37)
africa_travel <- terra::crop(travel_time_raster, africa_extent)
terra::plot(africa_travel, main = "Travel Time in Africa")
} # }
```
