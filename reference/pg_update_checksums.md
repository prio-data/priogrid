# Regenerate pgchecksum from locally verified files

Developer-facing function that recomputes MD5 checksums for all raw
source files currently present in the raw data folder and saves them to
`data/pgchecksum.rda`. Replaces the manual `data_raw/pgchecksum.R`
script.

## Usage

``` r
pg_update_checksums(only_present = TRUE)
```

## Arguments

- only_present:

  Logical. If TRUE (default), only compute checksums for files currently
  present in the raw folder. If FALSE, stops if any metadata file is
  missing locally.

## Value

A data.frame of checksums (invisibly). Also saves to
`data/pgchecksum.rda`.

## Details

Only run this when you have a fully verified, clean set of downloaded
files. The resulting `pgchecksum` object is bundled with the package and
used by
[`check_pgsourcefiles()`](http://prio-data.github.io/priogrid/reference/check_pgsourcefiles.md)
and the optional checksum verification in
[`get_pgfile()`](http://prio-data.github.io/priogrid/reference/get_pgfile.md).

## Examples

``` r
if (FALSE) { # \dontrun{
priogrid:::pg_update_checksums()
} # }
```
