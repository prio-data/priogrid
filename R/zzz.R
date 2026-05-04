.onLoad <- function(libname, pkgname) {
  # Set terra memory options if terra is available
  if (requireNamespace("terra", quietly = TRUE)) {
    terra::terraOptions(memmin = 5, memfrac = 0.8)
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("priogrid ", utils::packageVersion("priogrid"))

  # Remove legacy persisted config from previous priogrid versions
  legacy_config_file <- file.path(rappdirs::user_config_dir("R-priogrid", "prio"), "config.rds")
  if (file.exists(legacy_config_file)) {
    unlink(legacy_config_file)
    packageStartupMessage(
      "Note: Legacy persisted config from previous priogrid versions was removed. ",
      "Config is now session-scoped (pg_config() / pg_set_config()). ",
      "Only rawfolder persists (pg_set_rawfolder())."
    )
  }
}
.datatable.aware = TRUE
