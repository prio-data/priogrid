# Flag filenames that do not look like data files

Catches the two ways name resolution goes wrong in practice: a redirect
to a login endpoint, and a storage url whose path is an opaque object
key.

## Usage

``` r
pg_suspicious_filename(filename)
```

## Arguments

- filename:

  Character vector.

## Value

Logical vector.
