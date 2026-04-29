# Calculate elevation in PRIO-GRID format with set variable

Estimates either the min, max, or mean of elevation in each PRIO-GRID
cell based on vector data from GMTED2010.

## Usage

``` r
ruggedterrain_variable(variable, config = pg_current_config())
```

## Arguments

- variable:

  Character string indicating elevation function. Must be one of:
  "elevation_min", "elevation_max", "elevation_mean"

## Value

A `SpatRaster` object

## References

Danielson JJ, Gesch DB (2011). “Global Multi-resolution Terrain
Elevation Data 2010 (GMTED2010).” Technical Report 2011-1073, Earth
Resources Observation and Science (EROS) Center, Virginia.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get mean elevation for each PRIO-GRID cell
mean_elev <- ruggedterrain_variable(variable = "elevation_mean")

# Get minimum elevation for each PRIO-GRID cell
min_elev <- ruggedterrain_variable(variable = "elevation_min")

# Get maximum elevation for each PRIO-GRID cell
max_elev <- ruggedterrain_variable(variable = "elevation_max")
} # }
```
