# Reads the Li Nighttime data

Downloads, preprocesses, and harmonizes the Li et al. global nighttime
light dataset (v8). This dataset provides global, annual composites of
nighttime light intensity, harmonized across multiple satellite sensors
to produce a consistent multi-decadal time series.

## Usage

``` r
read_linight(overwrite_files = FALSE, config = pg_current_config())
```

## Arguments

- overwrite_files:

  Logical. If `TRUE`, previously fixed rasters are recalculated and
  overwritten. Defaults to `FALSE`.

## Value

A `SpatRaster` object

## Details

The function:

- Downloads the zipped Li Nighttime Lights raster files from the
  PRIO-GRID data repository

- Extracts TIF files, caching results to avoid repeated unzipping

- Identifies rasters with extent mismatches (common in the dataset)

- Resamples problematic rasters to a standardized global template
  (`EPSG:4326`, extent -180/180, -90/90) using nearest neighbor
  resampling

- Stores corrected rasters with a `"fixed_"` prefix for reuse

- Combines corrected rasters into a multi-layer `SpatRaster`

- Assigns layer names as dates, aligned to PRIO-GRID temporal
  conventions (January 1 of each year by default)

## Note

- Initial preprocessing (extent harmonization) may take time, but is
  cached for faster subsequent runs

- Large raster files may require substantial disk space and memory

- Nighttime lights are influenced by sensor calibration, atmospheric
  conditions, and moonlight; Li et al. provide harmonization but
  residual inconsistencies may remain

## References

Li X, Zhou Y, Zhao M, Zhao X (2020). “A Harmonized Global Nighttime
Light Dataset 1992–2018.” *Scientific Data*, **7**(1), 168. ISSN
2052-4463.
[doi:10.1038/s41597-020-0510-y](https://doi.org/10.1038/s41597-020-0510-y)
. <https://www.nature.com/articles/s41597-020-0510-y>.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read harmonized Li Nighttime Lights data
linight <- read_linight()

# Inspect structure
print(linight)

# Plot nighttime lights for year 2000
terra::plot(linight[["2000-01-01"]],
            main = "Global Nighttime Lights 2000")

# Compare change between 2000 and 2020
lights_2000 <- linight[["2000-01-01"]]
lights_2020 <- linight[["2020-01-01"]]
change <- lights_2020 - lights_2000
terra::plot(change, main = "Nighttime Lights Change 2000–2020")

# Extract regional time series
# example_extent <- terra::ext(100, 120, 20, 40) # East Asia
# region_lights <- terra::crop(linight, example_extent)
# terra::plot(region_lights[[1]], main = "Regional Nighttime Lights")
} # }
```
