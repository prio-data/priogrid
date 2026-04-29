# Reads the GHSL GHS Population Grid data

Downloads and processes GHSL (Global Human Settlement Layer) GHS
Population Grid data, which provides global population estimates at high
spatial resolution. The function extracts multi-temporal raster files
and formats them for compatibility with PRIO-GRID temporal structure.

## Usage

``` r
read_ghsl_population_grid(config = pg_current_config())
```

## Value

A `SpatRaster` object

## Details

GHSL GHS Population Grid provides population count estimates at 1 km
spatial resolution for multiple time periods globally. This function:

- Downloads zipped GHSL population raster files from the data repository

- Extracts TIF files from zip archives (cached to avoid repeated
  extraction)

- Loads and combines multiple raster layers into a single multi-temporal
  raster

- Standardizes layer names using PRIO-GRID date format for consistency

- Provides population data at 5-year intervals (typically 1975-2030)

## Note

- Files are automatically extracted from zip archives and cached locally

- The function handles large files and may take time for initial
  download

- Population estimates include both observed and modeled/projected
  values

- Temporal alignment uses PRIO-GRID month/day conventions for
  consistency

## References

Schiavina M, Freire S, MacManus K (2023). “GHS-POP R2023A - GHS
Population Grid Multitemporal (1975-2030).”
[doi:10.2905/2FF68A52-5B5B-4A22-8F40-C41DA8332CFE](https://doi.org/10.2905/2FF68A52-5B5B-4A22-8F40-C41DA8332CFE)
.
[2024-09-27](http://prio-data.github.io/priogrid/reference/2024-09-27).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read GHSL population grid data
# Warning: This involves large file downloads and processing
ghsl_population <- read_ghsl_population_grid()

# Examine the structure
print(ghsl_population)

# View available time periods
time_periods <- names(ghsl_population)
print(time_periods)

# Plot population for specific year
terra::plot(ghsl_population[["2020-12-31"]],
            main = "Global Population Distribution 2020")

# Compare population change over time
pop_1990 <- ghsl_population[["1990-12-31"]]
pop_2020 <- ghsl_population[["2020-12-31"]]
pop_change <- pop_2020 - pop_1990
terra::plot(pop_change, main = "Population Change 1990-2020")

# Extract population for specific region (example: crop to extent)
# europe_extent <- terra::ext(-10, 40, 35, 70)
# europe_pop <- terra::crop(ghsl_population, europe_extent)
# terra::plot(europe_pop[[nlyr(europe_pop)]], main = "Europe Population")

# Calculate total global population by year
global_pop_totals <- terra::global(ghsl_population, "sum", na.rm = TRUE)
years <- as.numeric(substr(names(ghsl_population), 1, 4))
plot(years, global_pop_totals$sum / 1e9,
     type = "l", main = "Global Population Trends",
     xlab = "Year", ylab = "Population (Billions)")
} # }
```
