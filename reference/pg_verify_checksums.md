# Compare downloaded files against the tested checksums

Advisory only. Files without a reference in
[pgchecksum](http://prio-data.github.io/priogrid/reference/pgchecksum.md)
keep `md5_ok` as NA and are not treated as problems, which is what makes
it possible to download a source for the first time and generate its
checksums afterwards.

## Usage

``` r
pg_verify_checksums(result, destfolder, quiet = FALSE)
```

## Arguments

- result:

  data.frame from
  [`pg_download_result()`](http://prio-data.github.io/priogrid/reference/pg_download_result.md).

- destfolder:

  Character. Raw data folder.

- quiet:

  Logical. Suppress progress messages.

## Value

`result` with `md5_ok` filled in where a reference exists.
