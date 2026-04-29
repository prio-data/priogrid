# Get configuration for an official PRIO-GRID release

Returns a
[`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md)
object with the spatial and temporal parameters for a known official
PRIO-GRID release. Useful for loading release data with the correct grid
dimensions and date range.

## Usage

``` r
pg_release_config(version = NULL, type = "05deg_yearly")
```

## Arguments

- version:

  Character string specifying the release version (e.g., "3.0.1"). If
  NULL, uses the current package version.

- type:

  Character string specifying the release type. Default: "05deg_yearly".

## Value

A `pg_config` object matching the release parameters.

## Examples

``` r
cfg <- pg_release_config("3.0.1")
cfg$nrow  # 360
#> [1] 360
```
