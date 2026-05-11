# Calculate distance to nearest international border (bdist2)

Computes the spherical distance (in kilometers) from each PRIO-GRID cell
centroid to the nearest international border using CShapes 2.0 boundary
data. The function calculates distances to shared borders between
countries, regardless of whether countries are separated by
international waters.

## Usage

``` r
bdist2(
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

  A list object from a previous `bdist2` calculation. If boundaries
  haven't changed, the function returns this result directly, avoiding
  recomputation. Default is NULL.

- config:

  A pg_config object, see
  [`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md).

- geodesic:

  Logical or NULL. If TRUE, uses spherical (S2/WGS84) geometry for
  distance calculations, projecting the result back to the config CRS.
  If FALSE, uses Euclidean distances in the config CRS, converting to
  meters via
  [`terra::linearUnits()`](https://rspatial.github.io/terra/reference/linearUnits.html).
  Default NULL auto-detects: geodesic for projected CRS (e.g. UTM),
  native for geographic CRS (e.g. WGS84, which is already geodesic via
  terra's internal handling).

## Value

A list containing three elements:

- `bdist2`: A `SpatRaster` with distances (km) from cell centroids to
  nearest international borders, masked to state system coverage

- `boundaries`: An `sf` object with country boundary line geometries

- `shared_borders`: An `sf` object with shared border segments between
  neighboring countries

## Details

The function includes optimization logic that reuses previous
calculations if country boundaries haven't changed since the last
computation, significantly reducing processing time for temporal
sequences.

Distance calculations use spherical geometry (S2) with WGS84 projection
for accurate measurements across the globe as default, particularly
important for high-latitude regions. Users can use euclidean distances
by setting geodesic = FALSE. This could be relevant, for instance with
UTM projections for small extents.

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
border_dist_2010 <- bdist2(as.Date("2010-01-01"))
} # }
```
