#' PRIO-GRID Meta-data survey and source information
#'
#' A (continuous, ongoing and incomplete) survey of
#' data we think is relevant for quantitative conflict research. We also use this
#' information to build the PRIO-GRID repository, i.e,. downloading and storing files,
#' reading locally downloaded files, map and read correct citation information, etc.
#' This is work in progress, and more will come here. If you want to suggest data to
#' this survey (your own or other data you think should be here), you can do that here:
#' <https://github.com/prio-data/priogrid/issues/new/choose>.
#'
#' You can search the meta-data, including specific bibliography entries like "author" or "journal"
#' using [pgsearch()].
#'
#' @format ## `pgsources`
#' A data frame with 37 rows and 18 columns:
#' \describe{
#'   \item{id}{A Unique Universal Identifier (UUID)}
#'   \item{source_name}{Required: Full name of the source, preferably including the institution hosting it.}
#'   \item{source_version}{Required: The version of the data source, as noted by the creator. If none, use the publication year.}
#'   \item{license}{Required: The data license. If nothing is stated, then "All rights are reserved". Use the URL to licenses that are specific to a owner.}
#'   \item{citation_keys}{Required: The bibkey(s) of the citation. We use Zotero with Better Bibtex to organize our citations. Separate with semi-colon if more than one.}
#'   \item{aws_bucket}{The Amazon S3 bucket to data repository. Use if source uses Amazon S3.}
#'   \item{aws_region}{The Amazon S3 region to data repository. Use if source uses Amazon S3.}
#'   \item{download_url}{The URL to the data-file. If multiple, we use a file in data/urls/\{id\}.txt with a URL per line.}
#'   \item{website_url}{Required: The URL to the most relevant online landing site for the data.}
#'   \item{tags}{Comma-separated tags used to sort and navigate the data sources.}
#'   \item{spatial_extent}{Required: World, Multiple continents, Single continent, or Several countries (spread). We are currently not including single country data.}
#'   \item{temporal_resolution}{Required: Static, Higher than monthly, Monthly, Quarterly, Yearly, or Less than yearly.}
#'   \item{reference_keys}{Bibkey(s) of other relevant references, e.g., articles discussing older versions, articles using the data, articles testing the data. Separate with semi-colon.}
#'   \item{prio_mirror}{URL or path to data/urls/\{id\}_mirror.txt. Alternative download location hosted by PRIO.}
#'   \item{download_url_exists}{Boolean, a test whether the supplied url worked. We will regularly test the urls.}
#'   \item{website_url_exists}{Boolean, a test whether the supplied url worked. We will regularly test the urls.}
#'   \item{prio_mirror_exists}{Boolean, a test whether the supplied url worked. We will regularly test the urls.}
#'   \item{created_at}{A date stamp when this entry was last updated.}
#' }
"pgsources"

#' PRIO-GRID File Checksums (MD5)
#'
#' We provide here file checksums for all the files we have used to test and build PRIO-GRID.
#' You can test if you have managed to download the same files using [check_pgsourcefiles()]
#' after you have downloaded the files using [download_pg_rawdata()].
#'
#' @format ## `pgchecksum`
#' A data frame with 22 rows and 5 columns:
#' \describe{
#'   \item{source_name}{Full name of the source, preferably including the institution hosting it.}
#'   \item{source_version}{The version of the data source, as noted by the creator. If none, use the publication year.}
#'   \item{id}{A Unique Universal Identifier (UUID)}
#'   \item{filename}{The filename of a file from the data source.}
#'   \item{md5}{The MD5 checksum of the correct data to use.}
#' }
"pgchecksum"


