# Build an official PRIO-GRID release

Sets config to specified configuration, calculates all variables to
custom location, then copies them to the official release folder. This
ensures consistency between custom and release workflows.

## Usage

``` r
build_release(
  version,
  type,
  nrow,
  ncol,
  crs,
  extent,
  temporal_resolution,
  start_date,
  end_date
)
```

## Arguments

- version:

  Character string with release version (e.g., "3.0.1")

- type:

  Character string with release type (e.g., "05deg_yearly")

- nrow:

  Number of rows

- ncol:

  Number of columns

- crs:

  Coordinate reference system

- extent:

  Named vector with xmin, xmax, ymin, ymax

- temporal_resolution:

  Temporal resolution string

- start_date:

  Start date (Date object)

- end_date:

  End date (Date object)

## Value

NULL (invisibly). Called for side effects (creating release).

## Examples

``` r
if (FALSE) { # \dontrun{
  build_release(
    version = "3.0.1",
    type = "05deg_yearly",
    nrow = 360,
    ncol = 720,
    crs = "epsg:4326",
    extent = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
    temporal_resolution = "1 year",
    start_date = as.Date("1850-12-31"),
    end_date = as.Date("2025-08-26")
 )
} # }
```
