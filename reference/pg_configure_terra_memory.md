# Configure terra memory settings based on config

Estimates memory usage and sets `terra::terraOptions(todisk = ...)`
accordingly. Only runs if terra is installed. Called internally from
terra-using functions.

## Usage

``` r
pg_configure_terra_memory(config = pg_current_config())
```

## Arguments

- config:

  A `pg_config` object.
