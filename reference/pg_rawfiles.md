# Extract url- and file-info from PRIO-GRID metadata

Extract url- and file-info from PRIO-GRID metadata

## Usage

``` r
pg_rawfiles(use_mirror = TRUE, only_file_extensions = FALSE)
```

## Arguments

- use_mirror:

  Boolean. Whether or not to use PRIO-GRID mirror.

## Value

data.frame

## Examples

``` r
file_info <- pg_rawfiles()
```
