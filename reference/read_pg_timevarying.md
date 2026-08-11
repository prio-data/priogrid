# Collect time-varying (non-static) PRIO-GRID data

Loads all time-varying variables and returns them either as a data.table
or as a list of rasters. See
[pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md)
for available variables.

## Usage

``` r
read_pg_timevarying(
  config = NULL,
  version = NULL,
  type = "05deg_yearly",
  spatial_hash = NULL,
  temporal_hash = NULL,
  years = NULL,
  start_date = NULL,
  end_date = NULL,
  pgids = NULL,
  extent = NULL,
  variables = NULL,
  as_raster = FALSE,
  test = FALSE,
  overwrite = FALSE,
  verify_checksums = FALSE
)
```

## Arguments

- config:

  A `pg_config` object for custom data, or NULL (default) for the
  official release.

- version:

  Character string specifying PRIOGRID version (release mode only).

- type:

  Character string specifying release type. Default: "05deg_yearly".

- spatial_hash:

  Character string with 6-character spatial hash (custom only).

- temporal_hash:

  Character string with 6-character temporal hash (custom only).

- years:

  Integer vector of years to keep. NULL (default) keeps all. Prunes hive
  partitions on the `year` column.

- start_date, end_date:

  Date (or Date-coercible) bounds on `measurement_date` (inclusive).
  NULL (default) leaves the respective bound open.

- pgids:

  Integer vector of PRIO-GRID cell ids to keep. NULL (default) keeps all
  cells.

- extent:

  Numeric `c(xmin, xmax, ymin, ymax)` in lon/lat (EPSG:4326), or a terra
  `SpatExtent` in the grid's native CRS; resolved to the pgids it covers
  and unioned with `pgids`. For non-4326 configs the lon/lat box is
  reprojected automatically. Requires `terra`. NULL (default) applies no
  spatial filter.

- variables:

  Character vector of time-varying variable columns to return (in
  addition to `pgid`, `measurement_date`, `year`). NULL (default)
  returns all variables.

- as_raster:

  Logical. If TRUE, returns list of SpatRasters. If FALSE (default),
  returns data.table.

- test:

  Logical. If TRUE, returns coverage summary data.frame.

- overwrite:

  Logical. If FALSE (default) and cached file exists, returns cached
  data. If TRUE, rebuilds from individual variables.

- verify_checksums:

  Logical. If TRUE, verifies checksums of cached files against stored
  MD5 values. Default FALSE.

## Value

data.table with pgid + measurement_date as rows and variables as
columns, or list of terra SpatRasters if as_raster=TRUE, or coverage
test data.frame if test=TRUE

## Details

The mode is determined by the `config` argument:

- `config = NULL` (default): loads from the official release (downloads
  if needed).

- `config = pg_config(...)`: loads from custom data built with that
  config.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Load official release as data.table
  pg_dt <- read_pg_timevarying()

  # Load specific official release
  pg_dt <- read_pg_timevarying(version = "3.0.1")

  # Load custom data
  cfg <- pg_config(nrow = 180, ncol = 360)
  pg_dt <- read_pg_timevarying(config = cfg)

  # Subset: two years, a bounding box, and one variable
  pg_dt <- read_pg_timevarying(version = "3.0.1", years = c(2010, 2011),
                               extent = c(10, 12, 50, 52), variables = "cru_tmp")
} # }
```
