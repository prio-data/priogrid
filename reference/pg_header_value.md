# Look up a response header

Returns the last occurrence, which after redirects is the one belonging
to the response that actually carried the data.

## Usage

``` r
pg_header_value(headers, name)
```

## Arguments

- headers:

  Named list from
  [`curl::parse_headers_list()`](https://jeroen.r-universe.dev/curl/reference/parse_headers.html).

- name:

  Character. Lower-case header name.

## Value

Character string, or NA if the header is absent.
