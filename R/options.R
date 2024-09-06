pgconfig_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))


#' PRIO-GRID Options Manager
#'
#' set_extent c(xmin, xmax, ymin, ymax)
#' set_crs a crs string
#' set_nrow integer number of rows in output raster
#' set_ncol integer number of columns in output raster
#' set_rawfolder the folder where rawdata should be downloaded (possibly very large files), defaults to temporary folder
#'
#' @examples
#' pgoptions <- PGOptionsManager$new()
#' pgoptions$set_rawfolder <- tempfolder()
#' pgoptions$set_verbose(FALSE)
PGOptionsManager <- R6::R6Class(
  "PGOptionsManager",
  public = list(
    initialize = function() {
      private$load_options()
    },

    set_extent = function(value) {
      private$options$extent <- terra::ext(value)
      private$save_options()
    },

    set_crs = function(value) {
      private$options$crs <- terra::crs(value)
      private$save_options()
    },

    set_nrow = function(value) {
      private$options$nrow <- as.integer(value)
      private$save_options()
    },

    set_ncol = function(value) {
      private$options$ncol <- as.integer(value)
      private$save_options()
    },

    set_rawfolder = function(value){
      private$options$rawfolder <- normalizePath(value)
      private$save_options()
    },

    set_verbose = function(value){
      private$options$verbose <- value
      private$save_options()
    },

    get_crs = function() private$options$crs,
    get_extent = function() private$options$extent,
    get_nrow = function() private$options$nrow,
    get_ncol = function() private$options$ncol,
    get_rawfolder = function() private$options$rawfolder,
    get_verbose = function() private$options$verbose,

    print = function() {
      cat("Current options:\n")
      cat("  nrow:", private$options$nrow, "\n")
      cat("  ncol:", private$options$ncol, "\n")
      cat("  crs:", private$options$crs, "\n")
      cat("  extent:", private$options$extent, "\n")
      cat("  rawfolder:", private$options$rawfolder, "\n")
      cat("  verbose:", private$options$verbose, "\n")
      invisible(self)
    }
  ),

  private = list(
    options = list(
      nrow = 360,
      ncol = 720,
      crs = "epsg:4326",
      extent = c("xmin" = -180, "xmax" =  180, "ymin" = -90, "ymax" = 90),
      rawfolder = tempdir(),
      verbose = TRUE
    ),

    load_options = function() {
      if (!cachem::is.key_missing(pgconfig_cache$get("config"))) {
        private$options <- pgconfig_cache$get("config")
      } else{
        private$save_options()
      }
    },

    save_options = function() {
      pgconfig_cache$set("config", private$options)
    }
  )
)

pgoptions <- PGOptionsManager$new()
if(pgoptions$get_verbose()) pgoptions$print()
