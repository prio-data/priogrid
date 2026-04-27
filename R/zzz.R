.onLoad <- function(libname, pkgname) {
  # Set terra memory options if terra is available
  if (requireNamespace("terra", quietly = TRUE)) {
    terra::terraOptions(memmin = 5, memfrac = 0.8)
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("priogrid ", utils::packageVersion("priogrid"))

  # Check for legacy persisted config and notify user
  legacy_dir <- rappdirs::user_config_dir("R-priogrid", "prio")
  if (dir.exists(legacy_dir)) {
    legacy_cache <- cachem::cache_disk(dir = legacy_dir)
    legacy_config <- legacy_cache$get("config")
    if (!cachem::is.key_missing(legacy_config)) {
      packageStartupMessage(
        "Note: Legacy persisted config detected from previous priogrid versions. ",
        "Only rawfolder is now persisted (via pg_set_rawfolder()). ",
        "All other settings are session-scoped (via pg_config() / pg_set_config())."
      )
      if (interactive()) {
        ans <- utils::askYesNo("Delete legacy config entry?")
        if (isTRUE(ans)) {
          legacy_cache$remove("config")
          packageStartupMessage("Legacy config entry deleted.")
        }
      }
    }
  }
}
.datatable.aware = TRUE
