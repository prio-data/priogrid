# Binary Mask for Grid Cells Intersecting International State System (cShapes 2.0)

Creates a binary raster mask indicating whether PRIO-GRID cells
intersect with country boundaries from the international state system
(CShapes 2.0) at a specified date, with optional minimum coverage
threshold.

## Usage

``` r
cshapes_cover(
  measurement_date,
  min_cover = 0.2,
  cshp = read_cshapes(),
  config = pg_current_config()
)
```

## Arguments

- measurement_date:

  A single `Date` object specifying the date for boundary analysis. Must
  be within CShapes temporal coverage.

- min_cover:

  Numeric. Minimum coverage threshold (0-1). Grid cells with state
  coverage below this value are set to NA. Default is 0 (any
  intersection).

- cshp:

  An `sf` object containing CShapes 2.0 boundary data. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  if not provided.

## Details

This function builds on
[`cshapes_cover_share`](http://prio-data.github.io/priogrid/reference/cshapes_cover_share.md)
to create a binary classification of grid cells. It:

- Calculates proportional coverage using CShapes boundary data

- Applies a minimum coverage threshold (cells below threshold become NA)

- Returns a binary mask where non-NA values indicate state system
  intersection

## References

Schvitz G, Girardin L, Rüegger S, Weidmann NB, Cederman L, Gleditsch KS
(2022). “Mapping the International System, 1886-2019: The CShapes 2.0
Dataset.” *Journal of Conflict Resolution*, **66**(1), 144–161. ISSN
0022-0027.
[doi:10.1177/00220027211013563](https://doi.org/10.1177/00220027211013563)
.
[2024-11-22](http://prio-data.github.io/priogrid/reference/2024-11-22).

## Examples

``` r
if (FALSE) { # \dontrun{
# Binary mask for any state system intersection in 2010
state_mask_2010 <- cshapes_cover(as.Date("2010-01-01"))

# Require at least 25% coverage
substantial_state <- cshapes_cover(as.Date("2010-01-01"), min_cover = 0.25)
} # }
```
