# PRIO-GRID File Checksums (MD5)

We provide here file checksums for all the files we have used to test
and build PRIO-GRID. You can test if you have managed to download the
same files using
[`check_pgsourcefiles()`](http://prio-data.github.io/priogrid/reference/check_pgsourcefiles.md)
after you have downloaded the files using
[`download_pg_rawdata()`](http://prio-data.github.io/priogrid/reference/download_pg_rawdata.md).

## Usage

``` r
pgchecksum
```

## Format

### `pgchecksum`

A data frame with 22 rows and 5 columns:

- source_name:

  Full name of the source, preferably including the institution hosting
  it.

- source_version:

  The version of the data source, as noted by the creator. If none, use
  the publication year.

- id:

  A Unique Universal Identifier (UUID)

- filename:

  The filename of a file from the data source.

- md5:

  The MD5 checksum of the correct data to use.
