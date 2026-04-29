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
`n_present`, and `all_present`, or NULL if the raw data folder is not
set.

## Examples

``` r
if (FALSE) { # \dontrun{
pg_data_availability()
} # }
```
