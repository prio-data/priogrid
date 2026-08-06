# Format urls and filenames as url list file lines

The filename column is only written where it differs from what
[`pg_default_filename()`](http://prio-data.github.io/priogrid/reference/pg_default_filename.md)
would derive, so that url lists for sources with self-describing urls
stay single-column.

## Usage

``` r
pg_format_url_list(url, filename = NULL)
```

## Arguments

- url:

  Character vector of urls.

- filename:

  Character vector of filenames, or NULL.

## Value

Character vector of lines.
