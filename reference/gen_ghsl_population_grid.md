# Generate GHSL GHS Population Grid

Aggregates the high-resolution GHSL (Global Human Settlement Layer) GHS
Population Grid data to PRIO-GRID resolution for all available 5-year
intervals (1975–2030). This provides population counts per PRIO-GRID
cell.

## Usage

``` r
gen_ghsl_population_grid(config = pg_current_config())
```

## Value

A `SpatRaster` object

## Details

This operation can be computationally intensive and may take time
depending on system performance and the size of the underlying rasters.

A slight nearest neighbor resampling was applied to get the exact
PRIO-GRID extent.

## Note

- Aggregation uses sum to preserve total population counts

- Nearest-neighbor resampling is applied for spatial alignment

- Large rasters may require significant memory and processing time

## References

Schiavina M, Freire S, MacManus K (2023). “GHS-POP R2023A - GHS
Population Grid Multitemporal (1975-2030).”
[doi:10.2905/2FF68A52-5B5B-4A22-8F40-C41DA8332CFE](https://doi.org/10.2905/2FF68A52-5B5B-4A22-8F40-C41DA8332CFE)
.
[2024-09-27](http://prio-data.github.io/priogrid/reference/2024-09-27).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate PRIO-GRID level GHSL population rasters
ghsl_pg <- gen_ghsl_population_grid()

# Inspect structure
print(ghsl_pg)

# Plot population for 2020
terra::plot(ghsl_pg[["2020-12-31"]],
            main = "PRIO-GRID Population Distribution 2020")

# Compute total population per PRIO-GRID cell change over time
pop_1990 <- ghsl_pg[["1990-12-31"]]
pop_2020 <- ghsl_pg[["2020-12-31"]]
change <- pop_2020 - pop_1990
terra::plot(change, main = "Population Change 1990–2020")
} # }
```
