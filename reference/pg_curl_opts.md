# Curl handle options used for all PRIO-GRID downloads

Identifies the client to the data providers we download from, and makes
a stalled transfer fail fast so that it can be resumed instead of
hanging.

## Usage

``` r
pg_curl_opts()
```

## Value

Named list of options for
[`curl::new_handle()`](https://jeroen.r-universe.dev/curl/reference/handle.html).
