# PRIO-GRID Variables

These are the variables available in PRIO-GRID.

## Usage

``` r
pgvariables
```

## Format

### `pgvariables`

A data frame with 4 rows and 5 columns:

- name:

  Full name of the variable.

- static:

  Boolean. Whether the variable varies over time or not.

- source_ids:

  Comma-separated string. The Unique Universal Identifier (UUID) of the
  sources used to build the variable.

## Details

The static information is used when building PRIO-GRID, as there are
different naming conventions for the data depending on whether the data
is static or with a temporal-dimension.
