# Largest non-missing value, or NA if there are none

[`base::max()`](https://rdrr.io/r/base/Extremes.html) warns and returns
-Inf for an all-NA input.

## Usage

``` r
pg_max_or_na(x)
```

## Arguments

- x:

  Numeric vector.

## Value

Numeric scalar.
