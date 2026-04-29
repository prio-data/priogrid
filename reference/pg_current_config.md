# Get the current session configuration

Returns the current session-scoped PRIO-GRID config. If none has been
set, initializes with defaults from
[`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md).

## Usage

``` r
pg_current_config()
```

## Value

A `pg_config` object.
