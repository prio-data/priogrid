# Load a PRIO-GRID variable

Loads a PRIO-GRID variable from disk and returns it as a terra
SpatRaster. The mode is determined by the `config` argument:

## Usage

``` r
load_pgvariable(
  varname,
  config = NULL,
  version = NULL,
  type = "05deg_yearly",
  spatial_hash = NULL,
  temporal_hash = NULL
)
```

## Arguments

- varname:

  Character string with the variable name.

- config:

  A `pg_config` object for custom data, or NULL (default) for the
  official release.

- version:

  Character string specifying PRIOGRID version (e.g., "3.0.1"). Only
  used in release mode (config = NULL). Defaults to current package
  version.

- type:

  Character string specifying release type (e.g., "05deg_yearly"). Only
  used in release mode. Default: "05deg_yearly".

- spatial_hash:

  Character string with 6-character spatial hash. Requires
  temporal_hash. Loads from the specified custom folder directly.

- temporal_hash:

  Character string with 6-character temporal hash. Requires
  spatial_hash.

## Value

Terra SpatRaster object

## Details

- `config = NULL` (default): loads from the official release (downloads
  if needed).

- `config = pg_config(...)`: loads from custom data built with that
  config.

- `spatial_hash` + `temporal_hash`: loads from a specific custom folder
  by hash.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Load from current official release (default)
  r <- load_pgvariable("cshapes_gwcode")

  # Load from specific official release
  r <- load_pgvariable("cshapes_gwcode", version = "3.0.1")

  # Load from custom data
  cfg <- pg_config(nrow = 180, ncol = 360)
  r <- load_pgvariable("cshapes_gwcode", config = cfg)

  # Load from specific custom folder by hash
  r <- load_pgvariable("cshapes_gwcode",
                       spatial_hash = "ecf4dd",
                       temporal_hash = "727cca")
} # }
```
