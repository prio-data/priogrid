# Reads the Natural Earth Disputed Areas Dataset

Downloads and imports the Natural Earth 1:10m scale Admin 0 Breakaway
and Disputed Areas dataset. This dataset depicts disputed and de facto
political boundaries, including areas with contested sovereignty or
breakaway regions.

## Usage

``` r
read_ne_disputed_areas()
```

## Value

An `sf` object

## Details

The dataset shows boundaries based on **control (de facto)** rather than
**international recognition (de jure)**. It is intended for
visualization and cartographic purposes. The dataset does not include
temporal attributes, so no specific time validity is provided.

## Note

- Boundaries represent political disputes as mapped by Natural Earth;
  they may not reflect all perspectives or claims

- Dataset is not dated and does not provide historical change tracking

- Intended for cartographic and analytical use, not for legal boundary
  definitions

## References

Natural Earth (2024). “Admin 0 - Breakaway and Disputed Areas. Made with
Natural Earth. Free Vector and Raster Map Data.”

## Examples

``` r
if (FALSE) { # \dontrun{
# Read disputed areas polygons
disputed_areas <- read_ne_disputed_areas()

# Inspect structure
print(disputed_areas)

# Plot disputed areas
plot(sf::st_geometry(disputed_areas),
     col = "red", border = "darkred",
     main = "Natural Earth Disputed Areas")
} # }
```
