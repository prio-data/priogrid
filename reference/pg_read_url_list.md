# Read a PRIO-GRID url list file

Url list files in `inst/extdata/urls` hold one download url per line. A
line may optionally carry a tab-separated second column naming the file
to store the download under locally. That second column is only needed
for sources whose urls do not name the file they serve, such as
`https://ndownloader.figshare.com/files/17626052`. Use
[`pg_resolve_filenames()`](http://prio-data.github.io/priogrid/reference/pg_resolve_filenames.md)
to generate it.

## Usage

``` r
pg_read_url_list(path)
```

## Arguments

- path:

  Character. Path to the url list file.

## Value

data.frame with columns `url` and `filename`, where `filename` is NA for
lines that do not state one.
