# Resolve PRIO-GRID data mode and paths

Determines whether to use release or custom mode based on provided
parameters, and returns resolved settings. Does not perform any file
operations or downloads.

## Usage

``` r
resolve_pg_mode(
  config = NULL,
  version = NULL,
  type = "05deg_yearly",
  spatial_hash = NULL,
  temporal_hash = NULL,
  overwrite = FALSE
)
```

## Arguments

- config:

  A `pg_config` object for custom mode, or NULL for release mode.

- version:

  Character string specifying PRIOGRID version (release mode only).

- type:

  Character string specifying release type (release mode only).

- spatial_hash:

  Character string with 6-character spatial hash (custom mode).

- temporal_hash:

  Character string with 6-character temporal hash (custom mode).

- overwrite:

  Logical. Whether to overwrite existing files.

## Value

A list with:

- mode:

  "release" or "custom"

- base_path:

  Resolved file path

- version:

  Resolved version (NULL if custom)

- type:

  Resolved type (NULL if custom)

- spatial_hash:

  Resolved spatial hash (NULL if release)

- temporal_hash:

  Resolved temporal hash (NULL if release)

- overwrite:

  Possibly modified overwrite flag

- warning:

  Warning message if any, otherwise NULL

- config:

  Resolved config for data construction

## Details

Mode is signalled by `config`:

- `config = NULL` (default) → release mode

- `config = pg_config(...)` → custom mode

- `spatial_hash` + `temporal_hash` → custom mode (explicit path
  override)
