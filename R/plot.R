# Internal: base-R colour vector for a pg_colormap string.
# Maps the pg_colormap vocabulary to verified grDevices::hcl.pals() palette names.
# @keywords internal
.pg_plot_palette <- function(colormap, n = 256L) {
  pal <- switch(as.character(colormap),
    viridis = "Viridis",
    inferno = "Inferno",
    rdbu_r  = "Blue-Red 3",   # diverging: low = blue, high = red
    tab20   = "Dark 3",       # qualitative
    "Viridis")
  grDevices::hcl.colors(n, pal)
}

# Internal: resolve display metadata (metatags first, pgvariables fallback).
# Prefers embedded pg_* metatags (terra::metags() -> data.frame), then the
# matching pgvariables row, then safe defaults. Derives colormap via
# .pg_colormap(plot_type) when not stamped.
# @keywords internal
.pg_plot_meta <- function(x, varname = NULL) {
  tags <- tryCatch(terra::metags(x), error = function(e) NULL)
  tv <- function(key) {
    if (is.null(tags) || !nrow(tags)) return(NA_character_)
    v <- tags$value[tags$name == key]
    if (length(v) == 0L) NA_character_ else v[[1]]
  }
  vn <- varname %||% { m <- tv("pg_name"); if (is.na(m)) names(x)[1] else m }
  row <- pgvariables[pgvariables$name == vn, ]
  pick <- function(key, col) {
    v <- tv(key)
    if (!is.na(v) && nzchar(v)) return(v)
    if (nrow(row) == 1L && !is.na(row[[col]]) && nzchar(as.character(row[[col]])))
      return(as.character(row[[col]]))
    NA_character_
  }
  label     <- pick("pg_label", "label");         if (is.na(label))     label     <- vn
  unit      <- pick("pg_unit", "unit");           if (is.na(unit))      unit      <- ""
  transform <- pick("pg_transform", "transform"); if (is.na(transform)) transform <- "identity"
  plot_type <- pick("pg_plot_type", "plot_type"); if (is.na(plot_type)) plot_type <- "continuous"
  colormap  <- tv("pg_colormap")
  if (is.na(colormap) || !nzchar(colormap)) colormap <- .pg_colormap(plot_type)
  num <- function(key) { v <- tv(key); if (is.na(v)) NA_real_ else suppressWarnings(as.numeric(v)) }
  list(varname = vn, label = label, unit = unit, transform = transform,
       plot_type = plot_type, colormap = colormap,
       value_min = num("pg_value_min"), value_max = num("pg_value_max"))
}

#' Plot a PRIO-GRID variable
#'
#' Renders a PRIO-GRID raster using display metadata from its embedded
#' \code{pg_*} GDAL metatags (written by \code{\link{save_pgvariable}()}),
#' falling back to the \code{\link{pgvariables}} table and then to safe
#' defaults. The backend is \code{terra::plot()}: native C++ rendering with
#' \code{maxcell} downsampling and no materialisation of the raster into a
#' data frame. No new package dependency — colours come from base
#' \code{grDevices::hcl.colors()}.
#'
#' @param x A \code{SpatRaster} (e.g. from \code{\link{load_pgvariable}()})
#'   or a character variable name (loaded via \code{load_pgvariable()}).
#' @param layer Integer. Which layer to draw for multi-layer rasters. Default 1.
#'   Can also be a string with the name of the layer. For timevarying rasters, the
#'   layer name is a date with the format "YYYY-MM-DD".
#' @param config A \code{\link{pg_config}} object. Passed to
#'   \code{\link{load_pgvariable}()} when \code{x} is a variable name string;
#'   ignored when \code{x} is already a \code{SpatRaster}.
#'   \code{NULL} (default) resolves via the release path or current config.
#' @param version Character. Release version string (e.g. \code{"3.0.1"});
#'   passed to \code{load_pgvariable()}. Cannot be combined with \code{config}.
#' @param type Character. Release type (e.g. \code{"05deg_yearly"});
#'   passed to \code{load_pgvariable()}. Default \code{"05deg_yearly"}.
#' @param spatial_hash,temporal_hash Six-character MD5 hashes that key a custom
#'   output folder; passed to \code{load_pgvariable()}. Must be supplied
#'   together. Cannot be combined with \code{version}.
#' @param ... Additional arguments forwarded to \code{terra::plot()}, overriding
#'   any metadata-derived default (e.g. \code{range}, \code{main}, \code{col}).
#'
#' @return The drawn \code{SpatRaster} layer, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Plot directly from a loaded raster:
#'   r <- load_pgvariable("ucdp_ged")
#'   plot_pgvariable(r)
#'
#'   # Plot by name from a custom data directory:
#'   cfg <- pg_config(nrow = 180, ncol = 360)
#'   plot_pgvariable("ucdp_ged", config = cfg)
#'
#'   # Plot a specific release:
#'   plot_pgvariable("ucdp_ged", version = "3.0.1")
#'
#'   # Override the title:
#'   plot_pgvariable(r, main = "Custom title")
#' }
plot_pgvariable <- function(x, layer = 1,
                            config = NULL,
                            version = NULL,
                            type = "05deg_yearly",
                            spatial_hash = NULL,
                            temporal_hash = NULL,
                            ...) {
  rlang::check_installed("terra", reason = "to plot PRIO-GRID variables")
  varname <- NULL
  if (is.character(x)) {
    varname <- x
    x <- load_pgvariable(x, config = config, version = version, type = type,
                         spatial_hash = spatial_hash, temporal_hash = temporal_hash)
  }

  meta <- .pg_plot_meta(x, varname = varname)
  r <- if (terra::nlyr(x) > 1L) x[[layer]] else x

  if (identical(meta$plot_type, "discrete")) {
    rf  <- terra::as.factor(r)
    ncl <- tryCatch(nrow(terra::cats(rf)[[1]]), error = function(e) 256L)
    if (is.null(ncl) || is.na(ncl) || ncl < 1L) ncl <- 256L
    args <- list(x = rf, type = "classes", main = meta$label,
                 col = .pg_plot_palette(meta$colormap, ncl),
                 legend = ncl <= 7L)
  } else {
    rr <- switch(meta$transform,
                 log1p = log1p(r), log10 = log10(r), sqrt = sqrt(r), r)
    main <- if (!identical(meta$transform, "identity"))
      paste0(meta$label, " (", meta$transform, " scale)") else meta$label
    args <- list(x = rr, main = main, col = .pg_plot_palette(meta$colormap, 256L))
    if (nzchar(meta$unit)) args$plg <- list(title = meta$unit)
    if (identical(meta$transform, "identity") &&
        is.finite(meta$value_min) && is.finite(meta$value_max))
      args$range <- c(meta$value_min, meta$value_max)
  }

  args <- utils::modifyList(args, list(...))
  do.call(terra::plot, args)
  invisible(args$x)
}
