# List custom PRIO-GRID data folders

Scans all custom data folders created by
[`calc_pg()`](http://prio-data.github.io/priogrid/reference/calc_pg.md)
and prints a summary of each configuration. Returns an indexed list of
[`pg_config()`](http://prio-data.github.io/priogrid/reference/pg_config.md)
objects for direct programmatic use.

## Usage

``` r
pg_list_custom()
```

## Value

An invisible named list of `pg_config` objects, one per custom folder.
Configs are indexed by position; use `[[i]]` to select one.

## Examples

``` r
if (FALSE) { # \dontrun{
  customs <- pg_list_custom()
  # [1] abc123/xyz789: nrow=360, ncol=720, 1850-12-31 to 2025-08-26, 3 vars

  read_pg_static(config = customs[[1]])
  calc_pg("new_var", config = customs[[2]])
} # }
```
