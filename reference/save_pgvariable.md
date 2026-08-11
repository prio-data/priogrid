# Save a PRIO-GRID variable

Saves a terra SpatRaster as a Cloud-Optimized GeoTIFF (COG) under the
`cog/` sub-directory of the dataset root. The variable must be listed in
[pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md).
Layer names, [`time()`](https://rdrr.io/r/stats/time.html), and
[`units()`](https://rdrr.io/r/base/units.html) are preserved natively
inside the `.tif`. Plot metadata from
[pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md)
(`label`, `unit`, `transform`, `plot_type`) is stamped as `pg_*` GDAL
metatags so the COG is self-describing. In addition, `pg_colormap`
(derived from `plot_type`), `pg_value_min`, `pg_value_max`,
`pg_value_mean`, `pg_value_std` (computed from the raster at save time),
and — for discrete variables — `pg_nunique` and `pg_class_values` are
also stamped. `value_*`, `nunique`, and `class_values` are computed from
the raster at save time; `colormap` is derived from `plot_type`. These
map directly onto rio-tiler/TiTiler `rescale` and `colormap_name`.

## Usage

``` r
save_pgvariable(rast, varname, save_to = pgout_path())
```

## Arguments

- rast:

  Terra SpatRaster object from a gen\_\*() function.

- varname:

  Character string with the variable name (must exist in pgvariables).

- save_to:

  Character string with the dataset root path. Defaults to current
  custom data location based on config.

## Value

NULL (invisibly). Called for side effects (saving file).

## Examples

``` r
if (FALSE) { # \dontrun{
  r <- gen_ne_disputed_area_share()
  save_pgvariable(r, "ne_disputed_area_share")
} # }
```
