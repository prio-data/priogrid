# Named continent bounding boxes: c(xmin, xmax, ymin, ymax) in WGS84 lon/lat.
# Keys are lowercase, space/underscore-stripped for flexible matching.
# @keywords internal
.pg_continent_extents <- list(
  africa       = c( -25,  52, -35,  38),
  antarctica   = c(-180, 180, -90, -60),
  asia         = c(  25, 180, -11,  77),
  europe       = c( -25,  65,  34,  72),
  northamerica = c(-170, -52,   7,  84),
  oceania      = c( 110, 180, -50,  10),
  southamerica = c( -82, -34, -56,  13)
)

# Internal: resolve extent to a numeric vector.
# Accepts numeric (passed through) or a continent name string (case-insensitive;
# spaces/underscores stripped). Errors with the list of valid names on mismatch.
# @keywords internal
.pg_resolve_extent <- function(extent) {
  if (is.numeric(extent)) return(extent)
  key <- gsub("[ _]", "", tolower(trimws(extent)))
  bbox <- .pg_continent_extents[[key]]
  if (is.null(bbox)) {
    valid <- paste(c("africa", "antarctica", "asia", "europe",
                     "north america", "oceania", "south america"),
                   collapse = ", ")
    stop("Unknown extent '", extent, "'. Valid continent names: ", valid, ".",
         call. = FALSE)
  }
  bbox
}

# Internal: compact "Author et al. (Year); ..." citation string for a variable.
# Follows the pgvariables -> pgsources -> REFERENCES.bib chain.
# @keywords internal
.pg_citation_string <- function(varname) {
  row <- pgvariables[pgvariables$name == varname, ]
  if (nrow(row) == 0L) return(NULL)

  source_ids <- trimws(unlist(strsplit(row$source_ids[[1]], ",")))
  src_rows   <- pgsources[pgsources$id %in% source_ids, ]

  # "Family et al. (Year)" from a person list + year string.
  inline_cite <- function(author, year) {
    if (is.null(author) || length(author) == 0L) return(NULL)
    family <- author[[1L]]$family
    if (is.null(family) || !nzchar(family)) return(NULL)
    suffix <- if (length(author) > 1L) " et al." else ""
    paste0(family, suffix, " (", if (nzchar(year %||% "")) year else "n.d.", ")")
  }

  cites <- character(0L)

  if (nrow(src_rows) > 0L) {
    bibkeys <- unique(trimws(unlist(strsplit(src_rows$citation_keys, ";"))))
    bibkeys <- bibkeys[nzchar(bibkeys)]
    if (length(bibkeys) > 0L) {
      bib <- tryCatch(get_bibliography(bibkeys), error = function(e) NULL)
      if (!is.null(bib) && length(bib) > 0L) {
        cites <- vapply(seq_along(bib), function(i) {
          raw <- unclass(bib[[i]])[[1L]]
          inline_cite(raw$author, raw$year) %||% ""
        }, character(1L))
        cites <- cites[nzchar(cites)]
      }
    }
  }

  # priogrid package citation — base-R bibentry; same person/year structure.
  pkg_cites <- tryCatch({
    pkg <- citation("priogrid")
    refs <- vapply(seq_along(pkg), function(i) {
      entry <- unclass(pkg)[[i]]
      inline_cite(entry$author, entry$year) %||% ""
    }, character(1L))
    refs[nzchar(refs)]
  }, error = function(e) character(0L))

  all_cites <- unique(c(cites, pkg_cites))
  if (length(all_cites) == 0L) return(NULL)
  paste(all_cites, collapse = "; ")
}


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
#' @param extent Numeric vector \code{c(xmin, xmax, ymin, ymax)} in lon/lat,
#'   or a continent name string: \code{"Africa"}, \code{"Antarctica"},
#'   \code{"Asia"}, \code{"Europe"}, \code{"North America"}, \code{"Oceania"},
#'   \code{"South America"} (case-insensitive; spaces/underscores optional).
#'   When \code{x} is a variable name string, passed to
#'   \code{load_pgvariable()} for a windowed COG read — only the blocks
#'   covering the requested region are fetched. Ignored with a warning when
#'   \code{x} is already a \code{SpatRaster}; crop it before calling if needed.
#' @param add_borders Logical. If \code{TRUE}, overlays country borders from
#'   \code{\link{read_cshapes}()} (requires the cShapes raw data). When the
#'   selected layer name is a parseable ISO date (e.g. \code{"1999-12-31"}),
#'   the historically correct borders for that date are used; otherwise the
#'   most recent snapshot (\code{max(gwsdate)}) is used as a fallback.
#' @param add_ne Logical. If \code{TRUE}, overlays land outlines from
#'   \code{\link{read_naturalearth_10m_land}()} (requires the Natural Earth
#'   raw data). Drawn beneath \code{add_borders} when both are \code{TRUE}.
#' @param ... Additional arguments forwarded to \code{terra::plot()}, overriding
#'   any metadata-derived default (e.g. \code{range}, \code{main}, \code{col}).
#'
#' @param add_citation Logical. If \code{TRUE}, renders a compact
#'   \dQuote{Author et al. (Year)} citation string in the bottom margin,
#'   sourced from \code{\link{pgvariables}}, \code{\link{pgsources}}, and
#'   \code{inst/REFERENCES.bib}. Variables with multiple data sources produce
#'   a semicolon-separated list. The bottom margin is expanded automatically
#'   to accommodate the text.
#'
#' @examples
#' \dontrun{
#'   # Plot directly from a loaded raster:
#'   r <- load_pgvariable("ucdp_ged")
#'   plot_pgvariable(r)
#'
#'   # Windowed read by continent name:
#'   plot_pgvariable("cru_tmp", extent = "Africa")
#'
#'   # Windowed read by numeric bbox (xmin, xmax, ymin, ymax):
#'   plot_pgvariable("cru_tmp", extent = c(-20, 20, 5, 20))
#'
#'   # Historical borders + land outline overlaid:
#'   plot_pgvariable("ucdp_ged", layer = "1999-12-31",
#'                   extent = "Africa", add_borders = TRUE, add_ne = TRUE)
#'
#'   # Custom data directory:
#'   cfg <- pg_config(nrow = 180, ncol = 360)
#'   plot_pgvariable("ucdp_ged", config = cfg)
#'
#'   # Specific release:
#'   plot_pgvariable("ucdp_ged", version = "3.0.1")
#'
#'   # Override terra::plot() args:
#'   plot_pgvariable(r, main = "Custom title")
#' }
plot_pgvariable <- function(x, layer = 1,
                            config = NULL,
                            version = NULL,
                            type = "05deg_yearly",
                            spatial_hash = NULL,
                            temporal_hash = NULL,
                            extent = NULL,
                            add_borders = FALSE,
                            add_ne = FALSE,
                            add_citation = FALSE,
                            ...) {
  rlang::check_installed("terra", reason = "to plot PRIO-GRID variables")
  if (!is.null(extent)) extent <- .pg_resolve_extent(extent)
  varname <- NULL
  if (is.character(x)) {
    varname <- x
    x <- load_pgvariable(x, config = config, version = version, type = type,
                         spatial_hash = spatial_hash, temporal_hash = temporal_hash,
                         extent = extent)
  } else if (!is.null(extent)) {
    warning("'extent' is ignored when 'x' is a SpatRaster; ",
            "crop before calling plot_pgvariable() if needed.", call. = FALSE)
  }

  meta <- .pg_plot_meta(x, varname = varname)
  r          <- if (terra::nlyr(x) > 1L) x[[layer]] else x
  layer_date <- tryCatch(as.Date(names(r)), error = function(e) as.Date(NA_character_))

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
    if (identical(meta$transform, "identity") &&
        is.finite(meta$value_min) && is.finite(meta$value_max))
      args$range <- c(meta$value_min, meta$value_max)
  }

  if (!is.na(layer_date))
    args$main <- paste0(args$main, " \u2014 ", format(layer_date))
  args <- utils::modifyList(args, list(...))

  # Pre-compute citation lines before plotting so margin can be sized correctly.
  cit_lines <- NULL
  if (add_citation) {
    old_mar <- graphics::par("mar")
    on.exit(graphics::par(mar = old_mar), add = TRUE)
    cit_raw <- tryCatch(.pg_citation_string(meta$varname),
                        error = function(e) {
                          warning("add_citation: ", conditionMessage(e), call. = FALSE)
                          NULL
                        })
    if (!is.null(cit_raw) && nzchar(cit_raw)) {
      # Wrap each per-reference line independently, then flatten.
      cit_lines <- unlist(lapply(
        strsplit(cit_raw, "\n")[[1L]],
        function(l) strwrap(l, width = 80L)
      ))
      cit_lines <- cit_lines[nzchar(trimws(cit_lines))]
    }
    n_cit  <- max(length(cit_lines), 1L)
    new_mar    <- old_mar
    new_mar[1] <- max(old_mar[1], 4.5 + n_cit)
    graphics::par(mar = new_mar)
  }

  do.call(terra::plot, args)

  if (add_ne || add_borders) {
    crop_to_extent <- function(sf_obj) {
      if (is.null(extent)) return(sf_obj)
      bbox <- sf::st_bbox(c(xmin = extent[1], xmax = extent[2],
                            ymin = extent[3], ymax = extent[4]),
                          crs = sf::st_crs(4326))
      suppressWarnings(sf::st_crop(sf_obj, bbox))
    }

    if (add_ne) {
      ne <- tryCatch(crop_to_extent(read_naturalearth_10m_land()),
                     error = function(e) {
                       warning("add_ne: ", conditionMessage(e), call. = FALSE); NULL
                     })
      if (!is.null(ne))
        plot(sf::st_geometry(ne), add = TRUE, border = "grey40", col = NA, lwd = 0.4)
    }

    if (add_borders) {
      cs <- tryCatch(read_cshapes(), error = function(e) {
        warning("add_borders: ", conditionMessage(e), call. = FALSE); NULL
      })
      if (!is.null(cs)) {
        d <- if (!is.na(layer_date)) layer_date else max(cs$gwsdate, na.rm = TRUE)
        borders <- dplyr::filter(cs, d %within% date_interval)
        borders <- tryCatch(crop_to_extent(borders), error = function(e) borders)
        plot(sf::st_geometry(borders), add = TRUE, border = "grey20", col = NA, lwd = 0.5)
      }
    }
  }

  if (!is.null(cit_lines) && length(cit_lines) > 0L) {
    n <- length(cit_lines)
    for (i in seq_along(cit_lines)) {
      # Stack lines bottom-to-top: last line sits at 4, each prior line 1 step higher.
      graphics::mtext(cit_lines[i], side = 1, line = 3.5 + (n - i),
                      cex = 0.55, col = "grey30", adj = 1)
    }
  }

  invisible(args$x)
}
