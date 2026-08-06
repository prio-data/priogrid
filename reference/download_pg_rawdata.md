# Download the raw-data for PRIO-GRID

Before running this, you need to set the folder using
pg_set_rawfolder("path/to/folder")

## Usage

``` r
download_pg_rawdata(
  file_info = NULL,
  overwrite = FALSE,
  batch_size = 20,
  max_retry = 10,
  max_concurrent = 4,
  retry_base_delay = 2,
  retry_max_delay = 120,
  verify = TRUE,
  quiet = !pg_current_config()$verbose
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

- batch_size:

  Integer. Number of files per download batch. Default 20.

- max_retry:

  Integer. Maximum number of retry attempts for failed downloads.
  Default 10.

- max_concurrent:

  Integer. Upper bound on simultaneous transfers. Default 4. A batch is
  transferred in parallel, so the effective batch is
  `min(batch_size, max_concurrent)` and this is the argument that
  decides how hard a server is hit.

- retry_base_delay:

  Numeric. Base seconds for exponential backoff between retries. Default
  2.

- retry_max_delay:

  Numeric. Maximum seconds to wait between retries. Default 120.

- verify:

  Logical. Whether to check downloaded files against the size the server
  announced and against
  [pgchecksum](http://prio-data.github.io/priogrid/reference/pgchecksum.md).
  Default TRUE.

- quiet:

  Logical. Suppress progress messages. Defaults to the inverse of
  `verbose` in
  [`pg_current_config()`](http://prio-data.github.io/priogrid/reference/pg_current_config.md).

## Value

data.frame (invisibly). One row per file in `file_info`, with columns
`source_name`, `source_version`, `id`, `filename`, `url`, `status`
("ok", "failed" or "skipped"), `attempts`, `status_code`, `bytes`,
`md5_ok`, `resolved_name`, `name_drift` and `error`.

## Details

Files are written to `<filename>.part` and only renamed into place once
the transfer has finished, so a file under its final name always means a
complete download. An interrupted run leaves `.part` files behind, and
calling the function again resumes them.

Two independent checks run on each downloaded file. The transfer is
checked against the size the server announced, and a short read is
retried. The file is separately compared against the MD5 in `pgchecksum`
where one exists; that comparison is advisory and never blocks a
download, because sources have no reference checksum until PRIO-GRID has
been built from them once. See
[`pg_update_checksums()`](http://prio-data.github.io/priogrid/reference/pg_update_checksums.md)
and
[`check_pgsourcefiles()`](http://prio-data.github.io/priogrid/reference/check_pgsourcefiles.md).

Files downloaded by earlier versions of this function were written
directly to their final name, so a file truncated before this change
still looks complete. Use
[`check_pgsourcefiles()`](http://prio-data.github.io/priogrid/reference/check_pgsourcefiles.md)
to find those.

## Examples

``` r
files_to_download <- pg_rawfiles() |> dplyr::filter(id == "ec3eea2e-6bec-40d5-a09c-e9c6ff2f8b6b")
# download_pg_rawdata(overwrite = TRUE, file_info = files_to_download)
```
