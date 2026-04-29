# Save a PRIO-GRID variable

Saves a terra SpatRaster as a wrapped .rds file. The variable must be
listed in
[pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md).

## Usage

``` r
save_pgvariable(rast, varname, save_to = pgout_path())
```

## Arguments

- rast:

  Terra SpatRaster object from a gen\_\*() function

- varname:

  Character string with the variable name (must exist in pgvariables)

- save_to:

  Character string with folder path. Defaults to current custom data
  location based on config.

## Value

NULL (invisibly). Called for side effects (saving file).

## Examples

``` r
if (FALSE) { # \dontrun{
  r <- gen_ne_disputed_area_share()
  save_pgvariable(r, "ne_disputed_area_share")
} # }
```
