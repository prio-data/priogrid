# Generate Raster for All Disputed Areas

Creates a PRIO-GRID raster representing the fraction of each cell
covered by **all types** of disputed areas from the Natural Earth Admin
0 Breakaway and Disputed Areas dataset. The dataset does not include
temporal information, so all disputes are treated as current/undated.

## Usage

``` r
gen_ne_disputed_area_share(config = pg_current_config())
```

## Value

A single-layer `SpatRaster` object

## Details

This function is a convenience wrapper around
[`ne_disputed_area_share`](http://prio-data.github.io/priogrid/reference/ne_disputed_area_share.md),
automatically using `type = "all"` to combine all disputed area types
into a single raster layer.

## References

Natural Earth (2024). “Admin 0 - Breakaway and Disputed Areas. Made with
Natural Earth. Free Vector and Raster Map Data.”

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate raster covering all disputed areas
rast <- gen_ne_disputed_area_share()
terra::plot(rast, main = "PRIO-GRID Coverage: All Disputed Areas")
} # }
```
