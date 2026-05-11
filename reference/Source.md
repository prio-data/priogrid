# Source Class

An R6 Class representing a data source with comprehensive validation.
Only for use in dev-mode (using devtools::load_all).

## Value

Boolean vector

character vector

character vector

tibble

text in console

## Methods

### Public methods

- [`Source$new()`](#method-Source-new)

- [`Source$save_url_files()`](#method-Source-save_url_files)

- [`Source$get_existing_tags()`](#method-Source-get_existing_tags)

- [`Source$get_existing_licenses()`](#method-Source-get_existing_licenses)

- [`Source$to_tibble()`](#method-Source-to_tibble)

- [`Source$print()`](#method-Source-print)

- [`Source$clone()`](#method-Source-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Source object

#### Usage

    Source$new(
      source_name,
      source_version,
      license,
      website_url,
      spatial_extent,
      temporal_resolution,
      citation_keys = NA_character_,
      aws_bucket = NA_character_,
      aws_region = NA_character_,
      download_url = NA_character_,
      prio_mirror = NA_character_,
      tags = NA_character_,
      reference_keys = NA_character_,
      bib_path = NULL
    )

#### Arguments

- `source_name`:

  String. Full name of the source

- `source_version`:

  String. Version of the source

- `license`:

  String. Data license

- `website_url`:

  String. URL to landing page

- `spatial_extent`:

  String. One of predefined spatial extents

- `temporal_resolution`:

  String. One of predefined temporal resolutions

- `citation_keys`:

  String. Optional. Bibkey(s) of citations

- `aws_bucket`:

  String. Optional. Amazon S3 bucket

- `aws_region`:

  String. Optional. Amazon S3 region

- `download_url`:

  String. Optional. URL to data file

- `prio_mirror`:

  String. Optional. Alternative download location

- `tags`:

  String. Optional. Comma-separated tags

- `reference_keys`:

  String. Optional. Other relevant bibkeys

- `bib_path`:

  String. Path to bibliography.

#### Examples

    new_source <- Source$new(
      source_name = "my new source",
      source_version = "v1.0",
      license = "CC BY 4.0",
      website_url = "www.example.com",
      spatial_extent = "World",
      temporal_resolution = "Yearly",
      citation_keys = "doeNewSource2025",
      download_url = "www.example.com/path/to/my/new/source/file.csv",
      tags = "test"
    )
    new_source # gives warning that doeNewSource2025 does not exist in the bibliography

------------------------------------------------------------------------

### Method `save_url_files()`

Saves url-files provided as a text-file with urls

#### Usage

    Source$save_url_files()

------------------------------------------------------------------------

### Method `get_existing_tags()`

Get tags from source data

#### Usage

    Source$get_existing_tags()

------------------------------------------------------------------------

### Method `get_existing_licenses()`

Get licenses from source data

#### Usage

    Source$get_existing_licenses()

------------------------------------------------------------------------

### Method `to_tibble()`

Converts source to a single row tibble

#### Usage

    Source$to_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints validation report of source.

#### Usage

    Source$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Source$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `Source$new`
## ------------------------------------------------

new_source <- Source$new(
  source_name = "my new source",
  source_version = "v1.0",
  license = "CC BY 4.0",
  website_url = "www.example.com",
  spatial_extent = "World",
  temporal_resolution = "Yearly",
  citation_keys = "doeNewSource2025",
  download_url = "www.example.com/path/to/my/new/source/file.csv",
  tags = "test"
)
#> Error: object 'Source' not found
new_source # gives warning that doeNewSource2025 does not exist in the bibliography
#> Error: object 'new_source' not found
```
