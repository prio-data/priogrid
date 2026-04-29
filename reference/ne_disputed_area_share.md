# Generate Raster for Disputed Areas by Type

Computes a PRIO-GRID raster representing the fraction of each cell
covered by disputed areas of a specified type, based on the Natural
Earth Admin 0 Breakaway and Disputed Areas dataset.

## Usage

``` r
ne_disputed_area_share(type, config = pg_current_config())
```

## Arguments

- type:

  Character string specifying the type of disputed area to include.
  Valid options are: "all", "breakaway", "disputed", "geo subunit", "geo
  unit", "indeterminate", "lease", or "overlay".

- disputed_areas:

  Optional `sf` object of disputed areas. Defaults to the result of
  [`read_ne_disputed_areas`](http://prio-data.github.io/priogrid/reference/read_ne_disputed_areas.md)().

## Value

A single-layer `SpatRaster` object

## References

Natural Earth (2024). “Admin 0 - Breakaway and Disputed Areas. Made with
Natural Earth. Free Vector and Raster Map Data.”

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate raster for "disputed" areas
disputed_raster <- ne_disputed_area_share("disputed")
terra::plot(disputed_raster, main = "PRIO-GRID Coverage: Disputed Areas")

# Generate raster combining all types
disputed_all <- ne_disputed_area_share("all")
terra::plot(disputed_all, main = "PRIO-GRID Coverage: All Disputed Areas")
} # }
```
