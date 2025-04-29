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

    #' @description Reset options to defaults.
    reset_options = function(){
      private$set_default_options()
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
      private$options$rawfolder <- normalizePath(value, mustWork = FALSE)
      if(!dir.exists(file.path(private$options$rawfolder, "tmp"))){
        dir.create(file.path(private$options$rawfolder, "tmp"), recursive = TRUE)
      }
      if(!dir.exists(file.path(private$options$rawfolder, "priogrid"))){
        dir.create(file.path(private$options$rawfolder, "priogrid"), recursive = TRUE)
      }
      normalizePath(private$options$rawfolder)
      private$save_options()
    },

    #' @description Set verbose output of functions if true.
    #' @param value Boolean
    set_verbose = function(value){
      private$options$verbose <- value
      private$save_options()
    },

    #' @description Set temporal resolution
    #' @param value String increment of temporal sequence. See [base::seq.Date] for more information.
    set_temporal_resolution = function(value){
      private$options$temporal_resolution <- value
      private$save_options()
    },

    #' @description Set start date
    #' @param value The date used globally to build PRIO-GRID. The month and day are used to define the
    #'   measurement date within the temporal resolution (e.g., as.Date(1900-06-30) would slice June 30 every year for compatible data sources).
    set_start_date = function(value){
      private$options$start_date <- value
      private$save_options()
    },

    #' @description Set end date
    #' @param value a date or "today"
    set_end_date = function(value){
      private$options$end_date <- value
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
    get_rawfolder = function(){
      if(is.na(private$options$rawfolder)){
        stop("Please set local folder for raw data using pgoptions$set_rawfolder(path).")
      } else{
        private$options$rawfolder
      }
    },
    #' @description Get verbose option
    get_verbose = function() private$options$verbose,
    #' @description Get temporal resolution option
    get_temporal_resolution = function() private$options$temporal_resolution,
    #' @description Get start date option
    get_start_date = function() as.Date(private$options$start_date),
    #' @description Get end date option
    get_end_date = function(){
      end_date <- private$options$end_date
      if(end_date == "today"){
        end_date <- Sys.Date()
      }
      return(end_date)
    },

    #' @description Prints all options
    print = function() {
      cat("Current options:\n")
      cat("  nrow:", private$options$nrow, "\n")
      cat("  ncol:", private$options$ncol, "\n")
      cat("  crs:", private$options$crs, "\n")
      cat("  extent:", as.vector(private$options$extent), "\n")
      cat("  rawfolder:", private$options$rawfolder, "\n")
      cat("  verbose:", private$options$verbose, "\n")
      cat("  temporal resolution:", private$options$temporal_resolution, "\n")
      cat("  start date:", private$options$start_date, "\n")
      cat("  end date:", private$options$end_date, "\n")
      invisible(self)
    }
  ),

  private = list(
    defaults = list(
      nrow = 360,
      ncol = 720,
      crs = "epsg:4326",
      extent = c("xmin" = -180, "xmax" =  180, "ymin" = -90, "ymax" = 90),
      rawfolder = NA,
      verbose = TRUE,
      temporal_resolution = "1 year",
      start_date = "1850-01-01",
      end_date = "today"
    ),

    options = list(),

    set_default_options = function(){
      pgconfig_cache$set("config", private$defaults)
    },

    load_options = function() {
      if (!cachem::is.key_missing(pgconfig_cache$get("config"))) {
        private$options <- pgconfig_cache$get("config")
      } else{
        private$set_default_options()
        private$options <- pgconfig_cache$get("config")
      }
    },

    save_options = function() {
      pgconfig_cache$set("config", private$options)
    }
  )
)
