#' Save an sf layer from the Puerto Rico WFS to disk
#'
#' Saves an `sf` object to disk with a smart filename based on the layer name
#' and the current date. Supports multiple geospatial formats via `sf::st_write()`.
#'
#' @param x An `sf` object to save.
#' @param layer_name Character. Layer name (e.g. `"g03_municipios"`).
#'   Used to build the output filename. If `NULL`, attempts to extract it
#'   from the object attribute or falls back to `"layer"`.
#' @param format Character. Output format. Options: `"gpkg"` (GeoPackage),
#'   `"geojson"`, `"shp"` (Shapefile), `"fgdb"` (File Geodatabase), `"csv"`.
#'   Default: `"gpkg"`.
#' @param dir Character. Output directory. Default: current working directory (`"."`).
#' @param filename Character. Custom filename (without extension).
#'   If `NULL` (default), auto-generated as `"pr_geodata_{layer_name}_{date}"`.
#' @param overwrite Logical. If `TRUE`, overwrites existing files. Default: `FALSE`.
#' @param add_date Logical. If `TRUE`, appends the current date to the filename.
#'   Default: `TRUE`.
#' @param quiet Logical. If `TRUE`, suppresses messages from `sf::st_write()`. Default: `FALSE`.
#' @param ... Additional arguments passed to `sf::st_write()`.
#'
#' @return Invisibly, the full path of the saved file.
#' @export
#'
#' @examples
#' \dontrun{
#' municipios <- geopr_get_layer("g03_municipios", all = TRUE)
#'
#' # Save as GeoPackage (default)
#' geopr_save(municipios, layer_name = "g03_municipios")
#'
#' # Save as GeoJSON in a specific folder
#' geopr_save(municipios, layer_name = "g03_municipios",
#'            format = "geojson", dir = "data/")
#'
#' # Custom filename
#' geopr_save(municipios, layer_name = "g03_municipios",
#'            filename = "municipios_pr", format = "gpkg")
#'
#' # Without date in filename
#' geopr_save(municipios, layer_name = "g03_municipios", add_date = FALSE)
#' }
geopr_save <- function(
    x,
    layer_name = NULL,
    format     = c("gpkg", "geojson", "shp", "fgdb", "csv"),
    dir        = ".",
    filename   = NULL,
    overwrite  = FALSE,
    add_date   = TRUE,
    quiet      = FALSE,
    ...
) {
  # --- Validation ---
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install it with install.packages('sf').", call. = FALSE)
  }
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }

  format <- match.arg(format)

  # --- Resolve layer_name ---
  if (is.null(layer_name)) {
    layer_name <- tryCatch(
      attr(x, "sf_column_name", exact = TRUE),
      error = function(e) NULL
    )
    if (is.null(layer_name)) layer_name <- "layer"
  }

  # Strip namespace prefix if present (e.g. "pr_geodata:g03_...")
  layer_name_clean <- sub("^[^:]+:", "", layer_name)

  # --- Build filename ---
  if (is.null(filename)) {
    base <- paste0("pr_geodata_", layer_name_clean)
    if (isTRUE(add_date)) {
      base <- paste0(base, "_", format(Sys.Date(), "%Y-%m-%d"))
    }
  } else {
    base <- filename
  }

  # --- Extension and driver by format ---
  ext_driver <- switch(
    format,
    gpkg    = list(ext = ".gpkg",    driver = "GPKG"),
    geojson = list(ext = ".geojson", driver = "GeoJSON"),
    shp     = list(ext = ".shp",     driver = "ESRI Shapefile"),
    fgdb    = list(ext = ".gdb",     driver = "OpenFileGDB"),
    csv     = list(ext = ".csv",     driver = "CSV")
  )

  # --- Create directory if it does not exist ---
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Directory created: ", normalizePath(dir, mustWork = FALSE))
  }

  # --- Final output path ---
  out_path <- file.path(dir, paste0(base, ext_driver$ext))
  out_path <- normalizePath(out_path, mustWork = FALSE)

  # --- Check overwrite ---
  if (file.exists(out_path) && !isTRUE(overwrite)) {
    stop(
      "File already exists: ", out_path,
      "\nUse `overwrite = TRUE` to overwrite it.",
      call. = FALSE
    )
  }

  # --- CSV warning (geometry is lost) ---
  if (format == "csv") {
    warning(
      "Saving as CSV: geometry will be converted to WKT and spatial information may be lost.",
      call. = FALSE
    )
    x$geometry_wkt <- sf::st_as_text(sf::st_geometry(x))
    x_to_write <- sf::st_drop_geometry(x)

    utils::write.csv(x_to_write, file = out_path, row.names = FALSE)

    if (!quiet) {
      message(
        sprintf(
          "Saved:   %s\nFormat:  %s | Rows: %s | Columns: %s",
          out_path, toupper(format), nrow(x_to_write), ncol(x_to_write)
        )
      )
    }
    return(invisible(out_path))
  }

  # --- Write with sf ---
  sf::st_write(
    obj    = x,
    dsn    = out_path,
    driver = ext_driver$driver,
    delete_dsn = isTRUE(overwrite),
    quiet  = quiet,
    ...
  )

  if (!quiet) {
    message(
      sprintf(
        "Saved:    %s\nFormat:   %s | Features: %s | CRS: %s",
        out_path,
        toupper(format),
        nrow(x),
        sf::st_crs(x)$input %||% "unknown"
      )
    )
  }

  invisible(out_path)
}

# Internal helper: null-coalesce operator
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(a)) a else b
