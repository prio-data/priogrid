# PRIO-GRID Meta-data survey and source information

A (continuous, ongoing and incomplete) survey of data we think is
relevant for quantitative conflict research. We also use this
information to build the PRIO-GRID repository, i.e,. downloading and
storing files, reading locally downloaded files, map and read correct
citation information, etc. This is work in progress, and more will come
here. If you want to suggest data to this survey (your own or other data
you think should be here), you can do that here:
<https://github.com/prio-data/priogrid/issues/new/choose>.

## Usage

``` r
pgsources
```

## Format

### `pgsources`

A data frame with 37 rows and 18 columns:

- id:

  A Unique Universal Identifier (UUID)

- source_name:

  Required: Full name of the source, preferably including the
  institution hosting it.

- source_version:

  Required: The version of the data source, as noted by the creator. If
  none, use the publication year.

- license:

  Required: The data license. If nothing is stated, then "All rights are
  reserved". Use the URL to licenses that are specific to a owner.

- citation_keys:

  Required: The bibkey(s) of the citation. We use Zotero with Better
  Bibtex to organize our citations. Separate with semi-colon if more
  than one.

- aws_bucket:

  The Amazon S3 bucket to data repository. Use if source uses Amazon S3.

- aws_region:

  The Amazon S3 region to data repository. Use if source uses Amazon S3.

- download_url:

  The URL to the data-file. If multiple, we use a file in
  data/urls/{id}.txt with a URL per line.

- website_url:

  Required: The URL to the most relevant online landing site for the
  data.

- tags:

  Comma-separated tags used to sort and navigate the data sources.

- spatial_extent:

  Required: World, Multiple continents, Single continent, or Several
  countries (spread). We are currently not including single country
  data.

- temporal_resolution:

  Required: Static, Higher than monthly, Monthly, Quarterly, Yearly, or
  Less than yearly.

- reference_keys:

  Bibkey(s) of other relevant references, e.g., articles discussing
  older versions, articles using the data, articles testing the data.
  Separate with semi-colon.

- prio_mirror:

  URL or path to data/urls/{id}\_mirror.txt. Alternative download
  location hosted by PRIO.

- download_url_exists:

  Boolean, a test whether the supplied url worked. We will regularly
  test the urls.

- website_url_exists:

  Boolean, a test whether the supplied url worked. We will regularly
  test the urls.

- prio_mirror_exists:

  Boolean, a test whether the supplied url worked. We will regularly
  test the urls.

- created_at:

  A date stamp when this entry was last updated.

## Details

You can search the meta-data, including specific bibliography entries
like "author" or "journal" using
[`pgsearch()`](http://prio-data.github.io/priogrid/reference/pgsearch.md).
