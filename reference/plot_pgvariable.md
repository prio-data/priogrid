# Plot a PRIO-GRID variable

Renders a PRIO-GRID raster using display metadata from its embedded
`pg_*` GDAL metatags (written by
[`save_pgvariable()`](http://prio-data.github.io/priogrid/reference/save_pgvariable.md)),
falling back to the
[`pgvariables`](http://prio-data.github.io/priogrid/reference/pgvariables.md)
table and then to safe defaults. The backend is
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html):
native C++ rendering with `maxcell` downsampling and no materialisation
of the raster into a data frame. No new package dependency — colours
come from base
[`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html).

## Usage

``` r
plot_pgvariable(
  x,
  layer = 1,
  config = NULL,
  version = NULL,
  type = "05deg_yearly",
  spatial_hash = NULL,
  temporal_hash = NULL,
  extent = NULL,
  add_borders = FALSE,
  add_ne = FALSE,
  add_citation = FALSE,
  ...
)
```

## Arguments

- x:

  A `SpatRaster` (e.g. from
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md))
  or a character variable name (loaded via
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md)).

- layer:

  Integer. Which layer to draw for multi-layer rasters. Default 1. Can
  also be a string with the name of the layer. For timevarying rasters,
  the layer name is a date with the format "YYYY-MM-DD".

- config:

  A
  [`pg_config`](http://prio-data.github.io/priogrid/reference/pg_config.md)
  object. Passed to
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md)
  when `x` is a variable name string; ignored when `x` is already a
  `SpatRaster`. `NULL` (default) resolves via the release path or
  current config.

- version:

  Character. Release version string (e.g. `"3.0.1"`); passed to
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md).
  Cannot be combined with `config`.

- type:

  Character. Release type (e.g. `"05deg_yearly"`); passed to
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md).
  Default `"05deg_yearly"`.

- spatial_hash, temporal_hash:

  Six-character MD5 hashes that key a custom output folder; passed to
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md).
  Must be supplied together. Cannot be combined with `version`.

- extent:

  Numeric vector `c(xmin, xmax, ymin, ymax)` in lon/lat, or a continent
  name string: `"Africa"`, `"Antarctica"`, `"Asia"`, `"Europe"`,
  `"North America"`, `"Oceania"`, `"South America"` (case-insensitive;
  spaces/underscores optional). When `x` is a variable name string,
  passed to
  [`load_pgvariable()`](http://prio-data.github.io/priogrid/reference/load_pgvariable.md)
  for a windowed COG read — only the blocks covering the requested
  region are fetched. Ignored with a warning when `x` is already a
  `SpatRaster`; crop it before calling if needed.

- add_borders:

  Logical. If `TRUE`, overlays country borders from
  [`read_cshapes()`](http://prio-data.github.io/priogrid/reference/read_cshapes.md)
  (requires the cShapes raw data). When the selected layer name is a
  parseable ISO date (e.g. `"1999-12-31"`), the historically correct
  borders for that date are used; otherwise the most recent snapshot
  (`max(gwsdate)`) is used as a fallback.

- add_ne:

  Logical. If `TRUE`, overlays land outlines from
  [`read_naturalearth_10m_land()`](http://prio-data.github.io/priogrid/reference/read_naturalearth_10m_land.md)
  (requires the Natural Earth raw data). Drawn beneath `add_borders`
  when both are `TRUE`.

- add_citation:

  Logical. If `TRUE`, renders a compact “Author et al. (Year)” citation
  string in the bottom margin, sourced from
  [`pgvariables`](http://prio-data.github.io/priogrid/reference/pgvariables.md),
  [`pgsources`](http://prio-data.github.io/priogrid/reference/pgsources.md),
  and `inst/REFERENCES.bib`. Variables with multiple data sources
  produce a semicolon-separated list. The bottom margin is expanded
  automatically to accommodate the text.

- ...:

  Additional arguments forwarded to
  [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html),
  overriding any metadata-derived default (e.g. `range`, `main`, `col`).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Plot directly from a loaded raster:
  r <- load_pgvariable("ucdp_ged")
  plot_pgvariable(r)

  # Windowed read by continent name:
  plot_pgvariable("cru_tmp", extent = "Africa")

  # Windowed read by numeric bbox (xmin, xmax, ymin, ymax):
  plot_pgvariable("cru_tmp", extent = c(-20, 20, 5, 20))

  # Historical borders + land outline overlaid:
  plot_pgvariable("ucdp_ged", layer = "1999-12-31",
                  extent = "Africa", add_borders = TRUE, add_ne = TRUE)

  # Custom data directory:
  cfg <- pg_config(nrow = 180, ncol = 360)
  plot_pgvariable("ucdp_ged", config = cfg)

  # Specific release:
  plot_pgvariable("ucdp_ged", version = "3.0.1")

  # Override terra::plot() args:
  plot_pgvariable(r, main = "Custom title")
} # }
```
