# Generate GHS-WUP-DEGURBA urban proportion grid

Generates PRIO-GRID level urbanization data using the GHS-WUP-DEGURBA
(Degree of Urbanisation) classification with a standard urban
definition. This function provides the proportion of urban area within
each PRIO-GRID cell for all available 5-year intervals (1975–2030).

## Usage

``` r
gen_ghs_wup_degurba_urban(config = pg_current_config())
```

## Value

A `SpatRaster` object with values ranging from 0 to 1

## Details

This function uses a predefined urban definition that includes:

- 30: Urban centres (cities)

- 23: Dense urban cluster

- 22: Semi-dense urban cluster

- 21: Suburban or peri-urban

Areas classified as rural (codes 10, 11, 12, 13) are considered
non-urban. The resulting raster provides values between 0 and 1,
representing the proportion of each PRIO-GRID cell that is classified as
urban.

This operation can be computationally intensive and may take time
depending on system performance and the size of the underlying rasters.

A slight nearest neighbor resampling was applied to get the exact
PRIO-GRID extent.

## Note

- Aggregation uses mean to calculate urban proportion per cell

- Nearest-neighbor resampling is applied for spatial alignment

- Large rasters may require significant memory and processing time

- For custom urban definitions, use
  [`ghs_wup_degurba`](http://prio-data.github.io/priogrid/reference/ghs_wup_degurba.md)
  directly

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

## See also

[`ghs_wup_degurba`](http://prio-data.github.io/priogrid/reference/ghs_wup_degurba.md)
for custom urban definitions

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate PRIO-GRID level urbanization rasters
urban_pg <- gen_ghs_wup_degurba_urban()

# Inspect structure
print(urban_pg)

# Plot urbanization for 2020
terra::plot(urban_pg[["2020-12-31"]],
            main = "PRIO-GRID Urban Proportion 2020")

# Compute urbanization change over time
urban_1990 <- urban_pg[["1990-12-31"]]
urban_2020 <- urban_pg[["2020-12-31"]]
change <- urban_2020 - urban_1990
terra::plot(change, main = "Urbanization Change 1990–2020")

# Identify highly urbanized cells (>75% urban)
highly_urban <- urban_pg[["2020-12-31"]] > 0.75
terra::plot(highly_urban, main = "Highly Urbanized Areas")

# Calculate global urban area trends
urban_stats <- terra::global(urban_pg, "mean", na.rm = TRUE)
years <- as.numeric(substr(names(urban_pg), 1, 4))
plot(years, urban_stats$mean,
     type = "l", main = "Global Urbanization Trends",
     xlab = "Year", ylab = "Mean Urban Proportion")
} # }
```
