# Create a PRIO-GRID configuration object

Constructs a configuration object with analysis parameters for
PRIO-GRID. Config objects are session-scoped and not persisted across
sessions. Use
[`pg_set_rawfolder()`](http://prio-data.github.io/priogrid/reference/pg_set_rawfolder.md)
for persistent machine-level settings.

## Usage

``` r
pg_config(
  nrow = 360L,
  ncol = 720L,
  crs = "epsg:4326",
  extent = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
  temporal_resolution = "1 year",
  start_date = as.Date("1850-12-31"),
  end_date = Sys.Date(),
  verbose = TRUE,
  automatic_download = TRUE
)
```

## Arguments

- nrow:

  Integer. Number of rows in the grid.

- ncol:

  Integer. Number of columns in the grid.

- crs:

  Character. Coordinate reference system string.

- extent:

  Numeric vector. Spatial extent c(xmin, xmax, ymin, ymax).

- temporal_resolution:

  Character. Increment of temporal sequence, see
  [base::seq.Date](https://rdrr.io/r/base/seq.Date.html).

- start_date:

  Date. The start date for temporal coverage. The month and day define
  the measurement date within the temporal resolution.

- end_date:

  Date or "today". The end date for temporal coverage.

- verbose:

  Logical. If TRUE, print informative messages.

- automatic_download:

  Logical. If TRUE, automatically download needed files.

## Value

A `pg_config` object (a named list with class `"pg_config"`).

## Examples

``` r
cfg <- pg_config(nrow = 180, ncol = 360)
cfg$nrow
#> [1] 180
```
