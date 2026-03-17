# zzz.R -- Executed automatically when the user calls library(geopr)
#
# Contains:
#   1. Internal cache environment shared across functions.
#   2. A startup message displayed when the package is loaded.
#   3. Check for optional suggested packages with installation instructions.

# ----------------------------------------------------------------------------
# 1. Internal cache environment
#    Used by geopr_list_layers() and geopr_get_layer() to avoid
#    repeated server calls within the same R session.
# ----------------------------------------------------------------------------
.geopr_cache <- new.env(parent = emptyenv())


# ----------------------------------------------------------------------------
# 2. Internal null-coalesce operator
#    Available to all package functions without importing from rlang.
# ----------------------------------------------------------------------------
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a[[1]]) && nzchar(a[[1]])) a else b
}


# ----------------------------------------------------------------------------
# 3. Startup message and optional dependency check
# ----------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "--------------------------------------------------\n",
    " geopr ", utils::packageVersion("geopr"),
    " -- Puerto Rico Geospatial Data\n",
    " Documentation: https://github.com/lanselotteoliveras/geopr\n",
    "--------------------------------------------------"
  )

  # Check optional packages (Suggests)
  optional_pkgs <- list(
    list(
      pkg  = "mapview",
      use  = "geopr_preview() with backend = \"mapview\" (recommended)",
      inst = "install.packages(\"mapview\")"
    ),
    list(
      pkg  = "leaflet",
      use  = "geopr_preview() with backend = \"leaflet\" (fallback)",
      inst = "install.packages(\"leaflet\")"
    ),
    list(
      pkg  = "htmltools",
      use  = "geopr_preview() for the map title",
      inst = "install.packages(\"htmltools\")"
    )
  )

  missing_pkgs <- Filter(
    function(p) !requireNamespace(p$pkg, quietly = TRUE),
    optional_pkgs
  )

  if (length(missing_pkgs) > 0) {
    msg_lines <- vapply(missing_pkgs, function(p) {
      sprintf("  * %s  ->  %s\n    Install with: %s", p$pkg, p$use, p$inst)
    }, character(1))

    packageStartupMessage(
      "\nOptional packages not installed:\n",
      paste(msg_lines, collapse = "\n"),
      "\n\nThese functions will fail until the packages are installed."
    )
  }
}


# ----------------------------------------------------------------------------
# 4. Cleanup on package detach (good practice)
# ----------------------------------------------------------------------------
.onDetach <- function(libpath) {
  rm(list = ls(envir = .geopr_cache), envir = .geopr_cache)
}
