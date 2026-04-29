# Collect static (non-time-varying) PRIO-GRID data

Loads all static variables and returns them either as a data.table or as
a list of rasters. See
[pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md)
for available variables.

## Usage

``` r
read_pg_static(
  config = NULL,
  version = NULL,
  type = "05deg_yearly",
  spatial_hash = NULL,
  temporal_hash = NULL,
  as_raster = FALSE,
  test = FALSE,
  overwrite = FALSE
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

- as_raster:

  Logical. If TRUE, returns list of SpatRasters. If FALSE (default),
  returns data.table.

- test:

  Logical. If TRUE, prints coverage summary for each variable.

- overwrite:

  Logical. If FALSE (default) and cached file exists, returns cached
  data. If TRUE, rebuilds from individual variables.

## Value

data.table with pgid as rows and variables as columns, or list of terra
SpatRasters if as_raster=TRUE

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
  pg_dt <- read_pg_static()

  # Load specific official release
  pg_dt <- read_pg_static(version = "3.0.1")

  # Load custom data
  cfg <- pg_config(nrow = 180, ncol = 360)
  pg_dt <- read_pg_static(config = cfg)

  # Load as rasters
  pg_rast <- read_pg_static(as_raster = TRUE)
} # }
```
