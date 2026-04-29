# Read GlobalDataLab Subnational Human Development Index (SHDI) Data

Downloads from a PRIO hosted mirror.

## Usage

``` r
read_shdi_csv()
```

## Value

A `data.frame` (tibble-compatible) containing SHDI values across
countries, subnational units, and years as provided by GlobalDataLab.

## Details

This function reads the GlobalDataLab Subnational Human Development
Index (SHDI) dataset (version 7.0) from local storage using PRIO-GRID
file management. The dataset is returned as a cleaned tabular object
with standardized lower-case column names.

## References

GlobalDataLab (2019). “The Subnational Human Development Database.”
[2024-10-23](http://prio-data.github.io/priogrid/reference/2024-10-23).

## See also

[`get_pgfile`](http://prio-data.github.io/priogrid/reference/get_pgfile.md)
for file retrieval and storage management

## Examples

``` r
if (FALSE) { # \dontrun{
shdi_csv <- read_shdi_csv()
} # }
```
