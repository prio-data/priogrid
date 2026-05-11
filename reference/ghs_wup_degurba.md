# Apply urban classification to GHS-WUP-DEGURBA data

Reclassifies the GHS-WUP-DEGURBA data into binary urban/non-urban
categories based on a user-defined set of DEGURBA classification codes.
The function aggregates the resulting binary classification to PRIO-GRID
resolution, providing the proportion of urban area within each PRIO-GRID
cell.

## Usage

``` r
ghs_wup_degurba(urban_definition, config = pg_current_config())
```

## Arguments

- urban_definition:

  Numeric vector of DEGURBA codes to classify as urban. Valid codes are:
  10, 11, 12, 13, 21, 22, 23, 30. See Details for code meanings.

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

A `SpatRaster` object with values ranging from 0 to 1, representing the
proportion of urban area within each PRIO-GRID cell according to the
specified urban definition.

## Details

The DEGURBA classification codes represent:

- 30: Urban centres (cities)

- 23: Dense urban cluster

- 22: Semi-dense urban cluster

- 21: Suburban or peri-urban

- 13: Rural cluster

- 12: Low density rural

- 11: Very low density rural

- 10: Water bodies or uninhabited areas

The function performs the following operations:

- Reads the high-resolution GHS-WUP-DEGURBA data

- Reclassifies cells to 1 (urban) if they match `urban_definition`, 0
  otherwise

- Aggregates to PRIO-GRID resolution using mean (proportion of urban
  area)

- Applies nearest-neighbor resampling for exact PRIO-GRID alignment

A slight nearest neighbor resampling was applied to get the exact
PRIO-GRID extent.

## Note

- This operation can be computationally intensive for large rasters

- Temporary files are created during processing

- The aggregation uses mean to calculate urban proportion per cell

- Different urban definitions produce different urbanization estimates

## References

Schiavina M, Melchiorri M, Pesaresi M, Jacobs-Crisioni C, Dijkstra L
(2025). “GHS-WUP-DEGURBA R2025A – GHS-WUP DEGURBA Settlement Layers,
Application of the Degree of Urbanisation Methodology (Stage I) to
GHS-WUP-POP R2025A, Multitemporal (1975-2100).”
[doi:10.2905/1c049178-ab00-4bbc-b638-3e3c19daaacb](https://doi.org/10.2905/1c049178-ab00-4bbc-b638-3e3c19daaacb)
.

European Commission, Statistical Office of the European Union (2021).
“Applying the Degree of Urbanisation — A Methodological Manual to Define
Cities, Towns and Rural Areas for International Comparisons.”
[doi:10.2785/706535](https://doi.org/10.2785/706535) .

Jacobs-Crisioni C, Schiavina M, Alessandrini A, Dijkstra L (2025).
“Population by Degree of Urbanization and by Urban Agglomeration from
1950 to 2100.”
https://publications.jrc.ec.europa.eu/repository/handle/JRC144219.
[doi:10.2760/1419546](https://doi.org/10.2760/1419546) .
[2025-12-18](http://prio-data.github.io/priogrid/reference/2025-12-18).

## Examples

``` r
if (FALSE) { # \dontrun{
# Use only urban centres as urban definition
urban_strict <- ghs_wup_degurba(urban_definition = 30)

# Use broader urban definition (all urban and suburban areas)
urban_broad <- ghs_wup_degurba(urban_definition = c(21, 22, 23, 30))

# Plot comparison
terra::plot(urban_strict[["2020-12-31"]],
            main = "Strict Urban Definition (Centres Only)")
terra::plot(urban_broad[["2020-12-31"]],
            main = "Broad Urban Definition")

# Custom definition: exclude suburban areas
urban_custom <- ghs_wup_degurba(urban_definition = c(22, 23, 30))
} # }
```
