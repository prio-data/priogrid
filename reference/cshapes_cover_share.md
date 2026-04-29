# Calculate Grid Cell Coverage by International State System Boundaries (cShapes 2.0)

Computes the proportion of each PRIO-GRID cell that intersects with
country boundaries from the CShapes 2.0 dataset at a specified date. The
function performs spatial overlay analysis to determine how much of each
grid cell falls within internationally recognized state boundaries.

## Usage

``` r
cshapes_cover_share(
  measurement_date,
  cshp = read_cshapes(),
  config = pg_current_config()
)
```

## Arguments

- measurement_date:

  A single `Date` object specifying the date for which to calculate
  boundary coverage. Must be within the temporal range of the CShapes
  dataset (1886 onwards).

- cshp:

  An `sf` object containing CShapes 2.0 boundary data with properly
  formatted date intervals. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  if not provided.

## Details

This function uses CShapes 2.0 historical boundary data to calculate the
proportion of each PRIO-GRID cell covered by countries that were part of
the international state system at the measurement date. The function:

- Filters CShapes data to boundaries valid at the measurement date

- Performs exact area-weighted extraction using the exactextractr
  package

- Returns proportional coverage values (0-1) for each grid cell

- Masks out non-land areas from the final result

The coverage values represent the fraction of each PRIO-GRID cell area
that falls within internationally recognized state boundaries.

## References

Schvitz G, Girardin L, Rüegger S, Weidmann NB, Cederman L, Gleditsch KS
(2022). “Mapping the International System, 1886-2019: The CShapes 2.0
Dataset.” *Journal of Conflict Resolution*, **66**(1), 144–161. ISSN
0022-0027.
[doi:10.1177/00220027211013563](https://doi.org/10.1177/00220027211013563)
.
[2024-11-22](http://prio-data.github.io/priogrid/reference/2024-11-22).

## See also

[`read_cshapes`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
for loading CShapes boundary data,
[`prio_blank_grid`](http://prio-data.github.io/priogrid/reference/prio_blank_grid.md)
for PRIO-GRID structure,
[`exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
for area-weighted extraction

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate state system coverage for 2010
coverage_2010 <- cshapes_cover_share(as.Date("2010-01-01"))

# View the result
print(coverage_2010)

# Plot the coverage
terra::plot(coverage_2010,
            main = "cShapes coverage 2010",
            col = terrain.colors(100))

# Calculate coverage for different time periods
coverage_1950 <- cshapes_cover_share(as.Date("1950-01-01"))
coverage_2000 <- cshapes_cover_share(as.Date("2000-01-01"))

# Compare coverage over time
coverage_change <- coverage_2000 - coverage_1950
terra::plot(coverage_change,
            main = "Change in cShapes coverage 1950-2000")

} # }
```
