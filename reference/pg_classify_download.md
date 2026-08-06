# Classify the outcome of a download attempt

Separates failures worth retrying from ones that will never succeed, so
that a missing file does not burn every retry.

## Usage

``` r
pg_classify_download(report)
```

## Arguments

- report:

  data.frame with `success` and `status_code` columns.

## Value

Character vector of "ok", "permanent" or "transient".
