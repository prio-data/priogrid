# Generate Li Nighttime Light

Aggregates the high-resolution Li et al. harmonized global nighttime
lights dataset to PRIO-GRID resolution for all available years
(1992–2021). This produces PRIO-GRID cell-level averages of nighttime
light intensity, harmonized with PRIO-GRID’s spatial and temporal
structure.

## Usage

``` r
gen_linight_mean(config = pg_current_config())
```

## Value

A `SpatRaster` object

## Details

The function:

- Reads annual nighttime lights rasters via
  [`read_linight`](http://prio-data.github.io/priogrid/reference/read_linight.md)

- Aggregates 1 km nighttime light intensity values into PRIO-GRID cells
  using mean values

- Retains global temporal coverage (1992–2021) as a multi-layer
  `SpatRaster`

- Aligns precisely to PRIO-GRID spatial extent (resampling handled in
  [`read_linight`](http://prio-data.github.io/priogrid/reference/read_linight.md))

## Note

- Aggregation uses mean values to represent typical nighttime light
  intensity per PRIO-GRID cell

- For sum-based aggregation (e.g., total light output per cell), see
  [`robust_transformation`](http://prio-data.github.io/priogrid/reference/robust_transformation.md)
  with `agg_fun = "sum"`

- Large rasters may take time and memory to process

## References

Li X, Zhou Y, Zhao M, Zhao X (2020). “A Harmonized Global Nighttime
Light Dataset 1992–2018.” *Scientific Data*, **7**(1), 168. ISSN
2052-4463.
[doi:10.1038/s41597-020-0510-y](https://doi.org/10.1038/s41597-020-0510-y)
. <https://www.nature.com/articles/s41597-020-0510-y>.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate PRIO-GRID level Li Nighttime Lights data
linight_pg <- gen_linight_mean()

# Inspect structure
print(linight_pg)

# Plot mean nighttime lights for 2000
terra::plot(linight_pg[["2000-01-01"]],
            main = "PRIO-GRID Nighttime Lights (Mean, 2000)")

# Compare mean intensity change between 2000 and 2020
lights_2000 <- linight_pg[["2000-01-01"]]
lights_2020 <- linight_pg[["2020-01-01"]]
change <- lights_2020 - lights_2000
terra::plot(change, main = "Change in Mean Nighttime Lights 2000–2020")
} # }
```
