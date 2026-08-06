# Number of bytes the response says the finished file should have

A resumed (206) response reports only the remaining bytes in
Content-Length, so the total is taken from Content-Range where possible
and otherwise reconstructed from the resume offset. Returns NA whenever
the size cannot be established (chunked responses, compressed
transfers), in which case callers must skip the check rather than treat
the file as bad.

## Usage

``` r
pg_expected_bytes(headers, resumefrom, status_code)
```

## Arguments

- headers:

  Named list from
  [`pg_response_headers()`](http://prio-data.github.io/priogrid/reference/pg_response_headers.md).

- resumefrom:

  Numeric. Byte offset the request resumed from.

- status_code:

  Integer. HTTP status of the response.

## Value

Numeric number of bytes, or NA.
