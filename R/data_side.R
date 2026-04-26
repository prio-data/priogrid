#' Read SIDE Ethnic Map Metadata and Local Files
#'
#' Downloads or locates the raw SIDE ethnic raster files registered in PRIO-GRID
#' metadata, then joins those files to SIDE's map metadata and the bundled
#' annual `leda_matches` table.
#'
#' @details
#' The function uses the SIDE source entry in [pgsources] to retrieve the raw
#' ASCII rasters from the ETH SIDE server when they are missing locally. It
#' returns the local file paths together with map metadata from
#' `sidedata::side_metadata()`, restricted to the ethnic marker.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item `meta`: a tibble of SIDE ethnic map metadata with local raster paths
#'   \item `matches`: the bundled annual SIDE-LEDA match table
#' }
#' @export
#'
#' @references
#' \insertRef{spatial-data-on-ethnicity}{priogrid}
read_side <- function() {
  files <- get_pgfile(
    source_name = "ETH SIDE",
    source_version = "v1",
    id = "e42b30e3-75da-4dd4-a375-0d6557087804"
  )

  side_meta <- sidedata::side_metadata() |>
    dplyr::filter(marker == "ethnic") |>
    dplyr::mutate(
      sideid = as.character(sideid),
      iso3c = .side_country_to_iso3c(country),
      file = files[match(paste0(sideid, ".asc"), basename(files))]
    ) |>
    dplyr::filter(!is.na(file))

  list(
    meta = side_meta,
    matches = leda_matches
  )
}

# Normalize SIDE group labels before joining the packaged match table to the
# raster metadata.
.normalize_side_group <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("[[:punct:]]", " ", x)
  gsub("\\s+", " ", x)
}

# Convert SIDE country names to ISO codes with a few historical aliases handled
# explicitly.
.side_country_to_iso3c <- function(x) {
  countrycode::countrycode(
    x,
    origin = "country.name",
    destination = "iso3c",
    custom_match = c(
      "Ivory Coast" = "CIV",
      "Congo (DRC)" = "COD",
      "Swaziland" = "SWZ",
      "Macedonia" = "MKD"
    )
  )
}

# Build a union template from all rasters contributing to one SIDE group so they
# can be aligned before averaging.
.side_union_template <- function(rasters) {
  extents <- lapply(rasters, terra::ext)
  xmin <- min(vapply(extents, terra::xmin, numeric(1)), na.rm = TRUE)
  xmax <- max(vapply(extents, terra::xmax, numeric(1)), na.rm = TRUE)
  ymin <- min(vapply(extents, terra::ymin, numeric(1)), na.rm = TRUE)
  ymax <- max(vapply(extents, terra::ymax, numeric(1)), na.rm = TRUE)
  xres <- min(vapply(rasters, function(r) terra::res(r)[1], numeric(1)), na.rm = TRUE)
  yres <- min(vapply(rasters, function(r) terra::res(r)[2], numeric(1)), na.rm = TRUE)

  terra::rast(
    ext = terra::ext(xmin, xmax, ymin, ymax),
    resolution = c(xres, yres),
    crs = terra::crs(rasters[[1]])
  )
}

# Align each raster to the group-specific template before averaging across SIDE
# map rounds.
.side_align_to_template <- function(r, template) {
  if (!terra::same.crs(r, template)) {
    terra::project(r, template, method = "bilinear")
  } else if (!terra::compareGeom(r, template, stopOnError = FALSE, crs = FALSE, rowcol = TRUE, ext = TRUE, res = TRUE)) {
    terra::resample(r, template, method = "bilinear")
  } else {
    r
  }
}

# Average one SIDE group across every available map for that country.
.side_average_group <- function(files) {
  rasters <- lapply(files, terra::rast)

  if (length(rasters) == 1) {
    return(rasters[[1]])
  }

  template <- .side_union_template(rasters)
  aligned <- lapply(rasters, .side_align_to_template, template = template)
  terra::app(terra::rast(aligned), fun = mean, na.rm = TRUE)
}

# Sum a list of rasters while preserving NA outside covered cells.
.side_sum_rasters <- function(rasters) {
  rasters <- rasters[!vapply(rasters, is.null, logical(1))]

  if (length(rasters) == 0) {
    return(NULL)
  }

  if (length(rasters) == 1) {
    return(rasters[[1]])
  }

  do.call(terra::mosaic, c(rasters, fun = "sum"))
}

# Build averaged SIDE group surfaces once, then reuse them across all requested
# years for the selected status.
.side_prepare_group_surfaces <- function(side_meta, matches) {
  needed_groups <- matches |>
    dplyr::distinct(iso3c, side_group) |>
    dplyr::mutate(side_group_norm = .normalize_side_group(side_group))

  meta_needed <- side_meta |>
    dplyr::mutate(side_group_norm = .normalize_side_group(groupname)) |>
    dplyr::semi_join(needed_groups, by = c("iso3c", "side_group_norm"))

  group_specs <- meta_needed |>
    dplyr::group_by(iso3c, side_group_norm) |>
    dplyr::summarise(files = list(file), .groups = "drop")

  surfaces <- stats::setNames(vector("list", nrow(group_specs)), paste(group_specs$iso3c, group_specs$side_group_norm, sep = "|||"))

  for (i in seq_len(nrow(group_specs))) {
    key <- paste(group_specs$iso3c[[i]], group_specs$side_group_norm[[i]], sep = "|||")
    surfaces[[key]] <- .side_average_group(group_specs$files[[i]])
  }

  surfaces
}

# Build one annual SIDE raster at native SIDE resolution by summing all country
# group shares that belong to the selected status in that year.
.side_build_year_raster <- function(year, status_matches, surfaces) {
  matches_year <- status_matches |>
    dplyr::filter(year == !!year) |>
    dplyr::mutate(side_group_norm = .normalize_side_group(side_group))

  countries <- sort(unique(matches_year$iso3c))
  country_rasters <- vector("list", length(countries))

  for (i in seq_along(countries)) {
    iso3c_code <- countries[[i]]
    groups <- matches_year |>
      dplyr::filter(iso3c == iso3c_code) |>
      dplyr::distinct(side_group_norm) |>
      dplyr::pull(side_group_norm)

    group_rasters <- lapply(groups, function(group_norm) {
      surfaces[[paste(iso3c_code, group_norm, sep = "|||")]]
    })

    country_rasters[[i]] <- .side_sum_rasters(group_rasters)
  }

  .side_sum_rasters(country_rasters)
}

#' Generate PRIO-GRID SIDE Status Shares
#'
#' Aggregates annual SIDE ethnic settlement shares to PRIO-GRID for one status
#' category at a time. SIDE group surfaces are averaged across all available map
#' rounds for each group-country combination, then combined with the bundled
#' annual `leda_matches` table.
#'
#' @param status One of `"excluded"`, `"included"`, or `"irrelevant"`.
#'
#' @return A \code{SpatRaster} object at the current PRIO-GRID spatio-temporal
#' resolution.
#' @export
#'
#' @references
#' \insertRef{spatial-data-on-ethnicity}{priogrid}
gen_side <- function(status = c("excluded", "included", "irrelevant")) {
  status <- match.arg(status)

  side <- read_side()
  status_matches <- side$matches |>
    dplyr::filter(status3 == !!status)

  if (nrow(status_matches) == 0) {
    stop("No SIDE matches found for status '", status, "'.")
  }

  measurement_dates <- as.Date(lubridate::int_end(pg_date_intervals()))
  years_needed <- sort(unique(lubridate::year(measurement_dates)))
  surfaces <- .side_prepare_group_surfaces(side$meta, status_matches)

  annual_layers <- vector("list", length(years_needed))
  annual_layer_names <- as.character(as.Date(paste0(years_needed, "-12-31")))

  for (i in seq_along(years_needed)) {
    native_raster <- .side_build_year_raster(years_needed[[i]], status_matches, surfaces)

    if (is.null(native_raster)) {
      blank <- prio_blank_grid()
      blank <- blank * NA_real_
      annual_layers[[i]] <- blank
    } else {
      annual_layers[[i]] <- robust_transformation(native_raster, agg_fun = "mean")
    }
  }

  annual_stack <- terra::rast(annual_layers)
  names(annual_stack) <- annual_layer_names

  year_lookup <- match(lubridate::year(measurement_dates), years_needed)
  result_layers <- lapply(year_lookup, function(i) {
    if (is.na(i)) {
      blank <- prio_blank_grid()
      return(blank * NA_real_)
    }
    annual_stack[[i]]
  })
  result <- terra::rast(result_layers)
  names(result) <- as.character(measurement_dates)

  result
}

#' Generate SIDE Excluded Share
#'
#' @return A \code{SpatRaster} object
#' @export
gen_side_excluded <- function() {
  gen_side(status = "excluded")
}

#' Generate SIDE Included Share
#'
#' @return A \code{SpatRaster} object
#' @export
gen_side_included <- function() {
  gen_side(status = "included")
}

#' Generate SIDE Irrelevant Share
#'
#' @return A \code{SpatRaster} object
#' @export
gen_side_irrelevant <- function() {
  gen_side(status = "irrelevant")
}
