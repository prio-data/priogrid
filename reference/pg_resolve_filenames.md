# Resolve the real filenames behind a source's download urls

Some sources serve files from urls that do not name them, such as
`https://ndownloader.figshare.com/files/17626052`. Left alone, those
land on disk as `17626052`. This developer-facing helper asks each
server what the file is actually called and records the answer as a
tab-separated second column in the source's url list, so that
[`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md)
keeps deriving local paths offline and the bundled
[pgchecksum](http://prio-data.github.io/priogrid/reference/pgchecksum.md)
stays valid.

## Usage

``` r
pg_resolve_filenames(id, write = FALSE, delay = 1)
```

## Arguments

- id:

  Character. Source id (UUID), as in
  [pgsources](http://prio-data.github.io/priogrid/reference/pgsources.md).

- write:

  Logical. If TRUE, rewrite the url list file under `inst/extdata`.
  Defaults to FALSE so that the proposal can be inspected first.

- delay:

  Numeric. Seconds to wait between requests. Default 1.

## Value

data.frame of urls, current filenames and resolved filenames
(invisibly).

## Details

The name is taken from the `Content-Disposition` header when the server
sends one, then from the url the request ended up at after redirects,
then from the stated url. None of these is reliable for every provider:
sources behind a login redirect to the login page, and GitHub redirects
to storage urls with opaque object keys. Read the proposed table before
writing, which is why this is run by a maintainer and committed rather
than run by users.

## Examples

``` r
if (FALSE) { # \dontrun{
priogrid:::pg_resolve_filenames("d99fbea7-2a01-4221-b900-29a58d33f591")
priogrid:::pg_resolve_filenames("d99fbea7-2a01-4221-b900-29a58d33f591", write = TRUE)
} # }
```
