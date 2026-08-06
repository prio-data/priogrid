# Extract a filename from a Content-Disposition header

Handles both the RFC 6266 extended form (`filename*=UTF-8''name`) and
the plain form (`filename="name"`), preferring the extended one.

## Usage

``` r
pg_disposition_filename(disposition)
```

## Arguments

- disposition:

  Character. Content-Disposition header value.

## Value

Character filename, or NA.
