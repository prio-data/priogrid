pgconfig_cache <- cachem::cache_disk(dir = rappdirs::user_config_dir("R-priogrid", "prio"))


#' R6 Class to set and get package-wide options for PRIO-GRID.
#'
#' @description
#' Options are stored in a persistent file in AppData, see pgconfig_cache$info()
#'
#' @examples
#' pgoptions <- PGOptionsManager$new()
#' pgoptions$set_rawfolder <- tempfolder()
#' pgoptions$set_verbose(FALSE)
#' pgoptions$print()
PGOptionsManager <- R6::R6Class(
  "PGOptionsManager",
  public = list(
    #' @description Initialize options
    initialize = function() {
      private$load_options()
    },

    #' @description Set output spatial extent
    #' @param value A vector with extent, c(xmin, xmax, ymin, ymax)
    set_extent = function(value) {
      private$options$extent <- terra::ext(value)
      private$save_options()
    },

    #' @description Set output CRS
    #' @param value CRS string
    set_crs = function(value) {
      private$options$crs <- terra::crs(value)
      private$save_options()
    },

    #' @description Set number of rows in output raster
    #' @param value Integer
    set_nrow = function(value) {
      private$options$nrow <- as.integer(value)
      private$save_options()
    },

    #' @description Set number of cols in output raster
    #' @param value Integer
    set_ncol = function(value) {
      private$options$ncol <- as.integer(value)
      private$save_options()
    },

    #' @description Set the folder where raw-data should be downloaded (possibly very large files), defaults to temporary folder
    #' @param value String path to folder
    set_rawfolder = function(value){
      private$options$rawfolder <- normalizePath(value)
      private$save_options()
    },

    #' @description Set verbose output of functions if true.
    #' @param value Boolean
    set_verbose = function(value){
      private$options$verbose <- value
      private$save_options()
    },

    #' @description Get crs option
    get_crs = function() private$options$crs,
    #' @description Get extent option
    get_extent = function() private$options$extent,
    #' @description Get nrow option
    get_nrow = function() private$options$nrow,
    #' @description Get ncol option
    get_ncol = function() private$options$ncol,
    #' @description Get rawfolder option
    get_rawfolder = function() private$options$rawfolder,
    #' @description Get verbose option
    get_verbose = function() private$options$verbose,

    #' @description Prints all options
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
