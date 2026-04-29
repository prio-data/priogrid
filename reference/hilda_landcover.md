# Extract Land Cover Proportions from HILDA+ Data for PRIO-GRID Cells

Calculates the proportion of each PRIO-GRID cell covered by a specific
land cover type using HILDA+ global land use/land cover data. The
function performs spatial aggregation from high-resolution HILDA+
rasters (1km) to PRIO-GRID cells (see
[`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md)),
accounting for partial coverage using area-weighted averaging.

## Usage

``` r
hilda_landcover(landcovertype, config = pg_current_config())
```

## Arguments

- landcovertype:

  Numeric. The land cover class identifier to extract from HILDA+ data.
  Must be a valid land cover code according to the HILDA+ classification
  scheme (see Details for common codes).

  HILDA+ land cover class codes (for stable categories):

  - 00: Ocean

  - 11: Urban

  - 22: Cropland

  - 33: Pasture/rangeland

  - 44: Forest

  - 55: Unmanaged grass/shrubland

  - 66: Sparse/no vegetation

  - 77: Water

  - 99: No data

## Value

A `SpatRaster` object (terra package) with the same structure as
PRIO-GRID, where each cell contains the proportion (0-1) of that cell
covered by the specified land cover type. The raster contains:

- Values: Proportions ranging from 0 (no coverage) to 1 (full coverage)

- Layers: One layer per year matching HILDA+ and PRIO-GRID temporal
  coverage

- Layer names: Dates in YYYY-MM-DD format

## Note

- Temporary files can be created during processing but are automatically
  cleaned up.

## References

Winkler K, Fuchs R, Rounsevell MDA, Herold M (2020). “HILDA+ Global Land
Use Change between 1960 and 2019.”
[doi:10.1594/PANGAEA.921846](https://doi.org/10.1594/PANGAEA.921846) .
[2025-05-26](http://prio-data.github.io/priogrid/reference/2025-05-26).

## See also

[`read_hilda`](http://prio-data.github.io/priogrid/reference/read_hilda.md)
for reading raw HILDA+ data,
[`prio_blank_grid`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)
for PRIO-GRID structure, https://ceos.org/gst/HILDAplus.html

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract cropland proportions
cropland_props <- hilda_landcover(landcovertype = 22)

# View the result
print(cropland_props)

# Plot cropland proportions for a specific year
terra::plot(cropland_props[["2000-12-31"]],
            main = "Cropland Proportion 2000")
} # }
```
