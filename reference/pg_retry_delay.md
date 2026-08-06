# Seconds to wait before the next download attempt

Exponential backoff with jitter, so that a server having a bad minute is
not hammered and concurrent clients do not retry in lockstep. A
`Retry-After` from the server raises the delay, but nothing exceeds
`cap`.

## Usage

``` r
pg_retry_delay(attempt, base = 2, cap = 120, retry_after = NA_real_)
```

## Arguments

- attempt:

  Integer. Attempt number, starting at 1.

- base:

  Numeric. Base delay in seconds.

- cap:

  Numeric. Maximum delay in seconds.

- retry_after:

  Numeric. Seconds requested by the server, or NA.

## Value

Numeric delay in seconds.
