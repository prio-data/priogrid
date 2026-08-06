# Build an empty per-file download summary

Build an empty per-file download summary

## Usage

``` r
pg_download_result(file_info, status)
```

## Arguments

- file_info:

  data.frame of files, as from
  [`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md).

- status:

  Character. Initial status for every row.

## Value

data.frame with the columns documented in
[`download_pg_rawdata()`](http://prio-data.github.io/priogrid/reference/download_pg_rawdata.md).
