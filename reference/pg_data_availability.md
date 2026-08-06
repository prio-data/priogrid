# Check which PRIO-GRID raw data files are available locally

Returns a summary of which data sources have been downloaded to the raw
data folder. Useful for checking data status before running
compute-heavy functions.

## Usage

``` r
pg_data_availability()
```

## Value

A data.frame with columns `source_name`, `source_version`, `n_files`,
`n_present`, `n_partial`, and `all_present`, or NULL if the raw data
folder is not set. `n_partial` counts files whose download was
interrupted and can be resumed by re-running
[`download_pg_rawdata()`](http://prio-data.github.io/priogrid/reference/download_pg_rawdata.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pg_data_availability()
} # }
```
