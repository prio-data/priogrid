# Reads the GHS-WUP-DEGURBA data

Downloads and processes GHS-WUP-DEGURBA (World Urbanization Prospects -
Degree of Urbanisation) data, which provides global urbanization levels
classified according to the Degree of Urbanisation methodology. The
function extracts multi-temporal raster files and formats them for
compatibility with PRIO-GRID temporal structure.

## Usage

``` r
read_ghs_wup_degurba(config = pg_current_config())
```

## Value

A `SpatRaster` object with DEGURBA classification codes

## Details

GHS-WUP-DEGURBA provides urbanization classification at 1 km spatial
resolution for multiple time periods globally. This function:

- Downloads zipped GHS-WUP-DEGURBA raster files from the data repository

- Extracts TIF files from zip archives (cached to avoid repeated
  extraction)

- Loads and combines multiple raster layers into a single multi-temporal
  raster

- Standardizes layer names using PRIO-GRID date format for consistency

- Provides urbanization classification data at 5-year intervals
  (typically 1975-2030)

The DEGURBA classification uses the following codes:

- 30: Urban centres (cities)

- 23: Dense urban cluster

- 22: Semi-dense urban cluster

- 21: Suburban or peri-urban

- 13: Rural cluster

- 12: Low density rural

- 11: Very low density rural

- 10: Water bodies or uninhabited areas

## Note

- Files are automatically extracted from zip archives and cached locally

- The function handles large files and may take time for initial
  download

- Classification includes both observed and modeled/projected values

- Temporal alignment uses PRIO-GRID month/day conventions for
  consistency

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
# Read GHS-WUP-DEGURBA data
# Warning: This involves large file downloads and processing
degurba <- read_ghs_wup_degurba()

# Examine the structure
print(degurba)

# View available time periods
time_periods <- names(degurba)
print(time_periods)

# Plot urbanization classification for specific year
terra::plot(degurba[["2020-12-31"]],
            main = "Global Urbanization Classification 2020")

# Compare urbanization change over time
urban_1990 <- degurba[["1990-12-31"]]
urban_2020 <- degurba[["2020-12-31"]]
terra::plot(urban_2020 - urban_1990,
            main = "Urbanization Change 1990-2020")

# Extract urbanization for specific region (example: crop to extent)
# asia_extent <- terra::ext(60, 140, -10, 50)
# asia_urban <- terra::crop(degurba, asia_extent)
# terra::plot(asia_urban[[nlyr(asia_urban)]], main = "Asia Urbanization")

# Calculate frequency of each urbanization class
freq_2020 <- terra::freq(degurba[["2020-12-31"]])
print(freq_2020)
} # }
```
