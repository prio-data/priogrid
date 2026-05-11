# Assign Gleditsch-Ward Country Codes to PRIO-GRID Cells

Determines the dominant country in each PRIO-GRID cell using CShapes 2.0
boundary data and assigns the corresponding Gleditsch-Ward country code.
For cells containing multiple countries, the country with the largest
area coverage is assigned, with ties broken by selecting the lowest
country code.

## Usage

``` r
cshapes_gwcode(
  measurement_date,
  cshp = read_cshapes(),
  config = pg_current_config()
)
```

## Arguments

- measurement_date:

  A single `Date` object specifying the date for country assignment.
  Must be within the temporal range of the CShapes dataset.

- cshp:

  An `sf` object containing CShapes 2.0 boundary data with
  Gleditsch-Ward country codes. Defaults to
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  if not provided.

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

A `SpatRaster` object

## Note

- Small countries or territories may not appear if they don't dominate
  any grid cells

- Future versions may include provisions for minority country
  representation

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
# Assign country codes for 2010
country_codes_2010 <- cshapes_gwcode(as.Date("2010-01-01"))

print(country_codes_2010)
} # }
```
