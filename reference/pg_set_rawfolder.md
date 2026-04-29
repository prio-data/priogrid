# Set the raw data folder path

Persists the raw data folder path across sessions. Creates `tmp` and
`priogrid` subdirectories if they don't exist.

## Usage

``` r
pg_set_rawfolder(path)
```

## Arguments

- path:

  Character string. Path to the folder for raw data storage.

## Value

The normalized path (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
pg_set_rawfolder("/data/priogrid_raw")
pg_rawfolder()
} # }
```
