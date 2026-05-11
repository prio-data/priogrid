# Calculate distance to nearest land-contiguous border (bdist1)

Computes the spherical distance (in kilometers) from each PRIO-GRID cell
centroid to the border of the nearest land-contiguous neighboring
country using CShapes 2.0 boundary data. This implies that cells in e.g.
Northern Denmark are measured to the border to Germany even if the
straight-line distance to Norway (across international waters) is
shorter. Cells belonging to island states with no contiguous neighboring
country (e.g., New Zealand) are coded as missing. Islands within states
are still measured.

## Usage

``` r
bdist1(
  measurement_date,
  cshp = read_cshapes(),
  past_result = NULL,
  config = pg_current_config(),
  geodesic = NULL
)
```

## Arguments

- measurement_date:

  A single `Date` object specifying the date for boundary analysis. Must
  be within CShapes temporal coverage.

- cshp:

  An `sf` object containing CShapes 2.0 boundary data. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  if not provided.

- past_result:

  A list object from a previous `bdist1` calculation. If boundaries
  haven't changed, the function returns this result directly, avoiding
  recomputation. Default is NULL.

- config:

  A pg_config object, see
  [`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md).

- geodesic:

  Logical or NULL. If TRUE, computes distances in WGS84 using spherical
  (S2) geometry and reprojects the result to the config CRS. If FALSE,
  uses Euclidean distances in the config CRS. Default NULL auto-detects:
  geodesic for projected CRS (e.g. UTM), native for geographic CRS (e.g.
  WGS84, which terra already handles geodesically).

## Value

A list containing two elements:

- `bdist1`: A `SpatRaster` with distances (km) from cell centroids to
  nearest international borders, masked to state system coverage

- `boundaries`: An `sf` object with country boundary line geometries

## Details

The function includes optimization logic that reuses previous
calculations if country boundaries haven't changed since the last
computation, significantly reducing processing time for temporal
sequences.

Distance calculations use spherical geometry (S2) for accurate
measurements across the globe, particularly important for high-latitude
regions.

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
# Calculate border distances for 2010
border_dist_2010 <- bdist1(as.Date("2010-01-01"))
} # }
```
