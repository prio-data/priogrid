# Test if MD5 checksums of local files are the same as a tested set of files

Here, we use
[pgchecksum](http://prio-data.github.io/priogrid/reference/pgchecksum.md),
which we created when testing PRIO-GRID, and test it against a similar
method for your own local files. This is to verify that you are using
the same files as we used to build PRIO-GRID.

## Usage

``` r
check_pgsourcefiles()
```

## Value

data.frame

## Examples

``` r
res <- check_pgsourcefiles()
#> Error in pg_rawfolder(): Raw data folder is not set. Use pg_set_rawfolder(path) to set it.
```
