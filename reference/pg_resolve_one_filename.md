# Ask a server what a single file is called

Requests only the first byte, so this is cheap even for very large
files.

## Usage

``` r
pg_resolve_one_filename(url)
```

## Arguments

- url:

  Character. A single url.

## Value

Character filename, or NA if the request failed.
