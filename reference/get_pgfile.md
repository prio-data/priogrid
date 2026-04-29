# Get file-path on local system to a data source in PRIO-GRID

To look up src_name and version in PRIO-GRID, see
[`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md).

## Usage

``` r
get_pgfile(source_name, source_version, id)
```

## Arguments

- src_name:

  character, the source name

- version:

  character, the version number

## Value

file path, string

## Examples

``` r
get_pgfile(source_name = "ETH ICR cShapes", source_version = "2.0", id = "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
#> Error in pg_rawfolder(): Raw data folder is not set. Use pg_set_rawfolder(path) to set it.
```
