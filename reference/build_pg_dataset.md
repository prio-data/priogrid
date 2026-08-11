# Build a PRIO-GRID hive-partitioned dataset

Memory-safe alternative to
[`read_pg_timevarying()`](http://prio-data.github.io/priogrid/reference/read_pg_timevarying.md)
that writes the hive partitioned parquet dataset and CSV bundle without
loading the full table. Useful for manual rebuild and re-upload
workflows.

## Usage

``` r
build_pg_dataset(
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

  A `pg_config` object for custom data, or NULL (default) for the
  official release.

- version:

  Character string specifying PRIOGRID version (release mode only).

- type:

  Character string specifying release type. Default: "05deg_yearly".

- spatial_hash:

  6-character MD5 hash of spatial options (advanced use).

- temporal_hash:

  6-character MD5 hash of temporal options (advanced use).

- overwrite:

  Logical. If TRUE, rebuilds even if output already exists.

## Value

Invisibly returns the base output path.
