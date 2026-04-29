# Path to store PRIOGRID results

Returns the file path where PRIOGRID data is stored. The path structure
depends on whether you're working with custom data or official releases.

## Usage

``` r
pgout_path(
  version = NULL,
  type = NULL,
  spatial_hash = NULL,
  temporal_hash = NULL,
  config = pg_current_config()
)
```

## Arguments

- version:

  Character string specifying PRIOGRID version. Use for official
  releases.

- type:

  Character string specifying release type (e.g., "05deg_yearly"). Use
  for official releases.

- spatial_hash:

  Character string with 6-character spatial hash. If NULL, computed from
  current config.

- temporal_hash:

  Character string with 6-character temporal hash. If NULL, computed
  from current config.

## Value

Character string with file path

## Examples

``` r
if (FALSE) { # \dontrun{
  # Custom data with current config
  pgout_path()

  # Custom data with specific hashes
  pgout_path(spatial_hash = "a3f2e1", temporal_hash = "9c8b7a")

  # Official release
  pgout_path(version = "3.0.1", type = "05deg_yearly")
} # }
```
