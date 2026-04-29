# Get temporal configuration hash

Creates a 6-character MD5 hash of the current temporal options from
config. Rounds end_date to avoid creating new hashes for small date
changes.

## Usage

``` r
get_temporal_hash(config = pg_current_config())
```

## Value

Character string with 6-character hash
