# Download and initialize official PRIO-GRID release

Downloads the official PRIO-GRID release data from the PRIO CDN and
extracts it to the local raw data folder. Only downloads if the file
doesn't exist or if overwrite=TRUE.

## Usage

``` r
download_priogrid(
  version = NULL,
  type = "05deg_yearly",
  overwrite = FALSE,
  list_releases = FALSE
)
```

## Arguments

- version:

  Character string with release version

- type:

  Character string with release type (default: "05deg_yearly")

- overwrite:

  Logical. If TRUE, re-downloads even if file exists.

## Value

NULL (invisibly). Called for side effects (downloading data).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Download latest official release
  download_priogrid()

  # Download specific release
  download_priogrid(version = "3.0.1", type = "05deg_yearly")

  # List releases
  download_priogrid(list_releases = TRUE)
} # }
```
