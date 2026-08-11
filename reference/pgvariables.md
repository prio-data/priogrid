# PRIO-GRID Variables

These are the variables available in PRIO-GRID.

## Usage

``` r
pgvariables
```

## Format

### `pgvariables`

A data frame with 38 rows and 7 columns:

- name:

  Full name of the variable.

- static:

  Boolean. Whether the variable varies over time or not.

- source_ids:

  Comma-separated string. The Unique Universal Identifier (UUID) of the
  sources used to build the variable.

- label:

  Human-readable display title for plots/legends.

- unit:

  Measurement unit for colorbar labels; empty if dimensionless.

- transform:

  Display transform applied before plotting; one of `identity`, `log1p`,
  `log10`, or `sqrt`.

- plot_type:

  Plot type hint; one of `continuous`, `positive_real`, `count`,
  `share`, or `discrete`.

Raster statistics
(`value_min`/`value_max`/`value_mean`/`value_std`/`nunique`/`class_values`)
and the derived `colormap` are not stored here; they are computed at
build and stamped into each COG as `pg_*` metatags.

## Details

The static information is used when building PRIO-GRID, as there are
different naming conventions for the data depending on whether the data
is static or with a temporal-dimension.
