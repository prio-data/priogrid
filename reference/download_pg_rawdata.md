# Download the raw-data for PRIO-GRID

Before running this, you need to set the folder using
pg_set_rawfolder("path/to/folder")

## Usage

``` r
download_pg_rawdata(
  file_info = NULL,
  overwrite = FALSE,
  batch_size = 20,
  max_retry = 10
)
```

## Arguments

- file_info:

  A data.frame with the same structure as the result from
  [`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md).
  If file_info is null (default), then file_info will be all data
  returned from
  [`pg_rawfiles()`](http://prio-data.github.io/priogrid/reference/pg_rawfiles.md).

- overwrite:

  Whether or not to download and overwrite files already in local
  folder.

- resume:

  If true, will also download files that did not finish download last
  time the function was run.

## Value

data.frame Download summary

## Examples

``` r
files_to_download <- pg_rawfiles() |> dplyr::filter(id == "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
# download_pg_rawdata(overwrite = TRUE, file_info = files_to_download)
```
