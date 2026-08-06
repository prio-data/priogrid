# Rename raw files left under a previous naming convention

When a source's url list gains explicit filenames, files already
downloaded under the name derived from the url would otherwise be
re-downloaded and the old copies orphaned. This renames them in place
instead.

## Usage

``` r
pg_migrate_rawfiles(file_info = NULL, dry_run = TRUE)
```

## Arguments

- file_info:

  A data.frame as from
  [`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md).
  Defaults to all of it.

- dry_run:

  Logical. If TRUE (default), only report what would be renamed.

## Value

data.frame of renames (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
priogrid:::pg_migrate_rawfiles()
priogrid:::pg_migrate_rawfiles(dry_run = FALSE)
} # }
```
