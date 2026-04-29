# Update the current session configuration

Modifies named fields of the current session config. Only the fields you
specify are changed; others are preserved.

## Usage

``` r
pg_set_config(...)
```

## Arguments

- ...:

  Named arguments matching
  [`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md)
  parameters.

## Value

The updated `pg_config` object (invisibly).

## Examples

``` r
pg_set_config(nrow = 180, ncol = 360)
pg_current_config()$nrow
#> [1] 180
```
