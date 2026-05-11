# Generate Excluded Ethnic Population Shares from SIDE

A convenience wrapper for
[`side`](http://prio-data.github.io/priogrid/reference/side.md) that
extracts the share of the local population belonging to politically
excluded ethnic groups.

## Usage

``` r
gen_side_excluded(config = pg_current_config())
```

## Arguments

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

A `SpatRaster` with excluded population shares for each PRIO-GRID cell.
See [`side`](http://prio-data.github.io/priogrid/reference/side.md) for
full documentation.

## See also

[`side`](http://prio-data.github.io/priogrid/reference/side.md) for full
documentation and parameters
