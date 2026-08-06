# Download one batch of files

Writes to `<destfile>.part` so that an interrupted transfer never leaves
a truncated file under the name callers treat as complete, and so that a
rerun can resume it.

## Usage

``` r
pg_download_batch(urls, destfiles, progress = TRUE)
```

## Arguments

- urls:

  Character vector of urls to download.

- destfiles:

  Character vector of final file paths.

- progress:

  Logical. Whether to show a progress bar.

## Value

data.frame with one row per requested file.

## Details

Note that
[`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
reports the *effective* url, i.e. the one reached after redirects, so
the request url cannot be used to match results back to requests.
`destfile` comes back exactly as passed and is used instead.

[`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
transfers everything it is given in parallel and exposes no connection
limit, so the number of urls passed in is what bounds concurrency.
Callers keep that number small rather than this function.
