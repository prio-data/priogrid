# Converts raster with variable to data.frame

Assumes that the name of the raster layer is the name of the variable if
static is true, otherwise, the user must supply the correct variable
name. If static is false, the name of the raster layer is assumed to be
the time variable.

## Usage

``` r
rast_to_df(rast, static = TRUE, varname = NULL, config = pg_current_config())
```

## Arguments

- rast:

  SpatRaster

- static:

  True if no temporal dimension, False else.

- varname:

  The variable name, only required if static is False.

- config:

  A `pg_config` object. Defaults to
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
ne <- gen_naturalearth_cover()
rast_to_df(ne)
} # }
```
