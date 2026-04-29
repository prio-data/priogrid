# Share of PRIO-GRID Cells Covered by Land (Natural Earth)

Computes the proportion of each PRIO-GRID cell that intersects with
land, based on the Natural Earth 1:10m Physical Land dataset. Returns a
raster layer aligned to PRIO-GRID resolution with values in the range
`[0, 1]` indicating land coverage share.

## Usage

``` r
gen_naturalearth_cover_share(config = pg_current_config())
```

## Value

A single-layer `SpatRaster` object

## Note

- Values of `0` correspond to ocean-only cells

- Values of `1` correspond to cells fully covered by land

- Intermediate values represent fractional coverage (e.g., coastal
  areas)

## References

Natural Earth (2024). “Land. 10m Physical. Made with Natural Earth. Free
Vector and Raster Map Data.”

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute PRIO-GRID land cover share
land_share <- gen_naturalearth_cover_share()

# Inspect values
summary(values(land_share))

# Plot global land cover share
terra::plot(land_share,
            main = "Share of PRIO-GRID Cells Covered by Land")

# Extract land share for a region (e.g., West Africa)
africa_extent <- terra::ext(-20, 20, 0, 20)
terra::plot(terra::crop(land_share, africa_extent),
            main = "Land Share in West Africa")
} # }
```
