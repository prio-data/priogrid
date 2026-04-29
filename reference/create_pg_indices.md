# Create matrix with index numbering conventions as for PRIO-GRID.

Create matrix with index numbering conventions as for PRIO-GRID.

## Usage

``` r
create_pg_indices(config = pg_current_config())
```

## Arguments

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

A ncol\*nrow matrix with integer indices.

## Examples

``` r
pg <- create_pg_indices()
```
