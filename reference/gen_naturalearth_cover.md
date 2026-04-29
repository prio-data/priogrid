# Land Mask for PRIO-GRID Cells (Natural Earth)

Generates a binary raster mask indicating which PRIO-GRID cells
intersect with land, based on the Natural Earth land dataset. By
default, any nonzero land coverage qualifies a cell as land. The minimum
proportion of land cover required to classify a cell as land can be
adjusted with `min_cover`.

## Usage

``` r
gen_naturalearth_cover(min_cover = 0, config = pg_current_config())
```

## Arguments

- min_cover:

  Numeric, default `0`. Minimum fraction of a grid cell that must be
  covered by land for the cell to be classified as land. Should be
  between `0` and `1`.

## Value

A single-layer `SpatRaster` object

## Details

The function:

- Calls
  [`gen_naturalearth_cover_share`](http://prio-data.github.io/priogrid/reference/gen_naturalearth_cover_share.md)
  to compute the fractional land cover of each grid cell

- Sets cells with land coverage below `min_cover` to `NA`

- Returns a raster aligned to PRIO-GRID resolution with non-`NA` values
  representing land cells

## References

Natural Earth (2024). “Land. 10m Physical. Made with Natural Earth. Free
Vector and Raster Map Data.”

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate default land mask
land_mask <- gen_naturalearth_cover()
terra::plot(land_mask, main = "PRIO-GRID Land Mask (Natural Earth)")

# Require 50% land cover to classify as land
land_mask50 <- gen_naturalearth_cover(min_cover = 0.5)
terra::plot(land_mask50, main = "PRIO-GRID Land Mask (≥50% Land)")
} # }
```
