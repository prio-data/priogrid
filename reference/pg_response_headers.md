# Parse the headers of one row of a curl download report

Parse the headers of one row of a curl download report

## Usage

``` r
pg_response_headers(report, i)
```

## Arguments

- report:

  data.frame from
  [`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html).

- i:

  Integer. Row number.

## Value

Named list of headers, empty if unavailable.
