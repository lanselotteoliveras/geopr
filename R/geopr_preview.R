#' Interactive preview of a Puerto Rico geospatial layer
#'
#' Generates an interactive map with a subset of features from a Puerto Rico
#' WFS layer. Designed for quick exploration: lets you inspect the shape,
#' extent, and attributes of a layer **before** downloading it in full with
#' [geopr_get_layer()].
#'
#' @param layer Layer to preview. Accepts three forms:
#'   \itemize{
#'     \item **Numeric**: the \code{layer_id} shown in \code{\link{geopr_list_layers}}
#'       (e.g. `5`).
#'     \item **Character (short name)**: layer name without namespace
#'       (e.g. `"g03_municipios"`).
#'     \item **Character (full name)**: with namespace included
#'       (e.g. `"pr_geodata:g03_municipios"`).
#'     \item **`sf` object**: if you already have the layer downloaded, pass it
#'       directly. In this case `n`, `bbox`, `cql_filter` and all connection
#'       parameters are ignored.
#'   }
#'
#' @param n Integer. Maximum number of features to download from the server for
#'   the preview. Default: `500`. Lowering this value speeds up loading;
#'   increasing it shows more data but may take longer. Ignored if `layer`
#'   is an `sf` object.
#'
#' @param wfs_url Character. Base URL of the Puerto Rico WFS service.
#'   Normally does not need to be changed.
#'   Default: `"http://geoserver2.pr.gov/geoserver/pr_geodata/wfs"`.
#'
#' @param serviceVersion Character. WFS protocol version to use.
#'   Default: `"2.0.0"`. Change only if the server requires a different version
#'   (e.g. `"1.1.0"`).
#'
#' @param namespace Character. GeoServer namespace prefix for Puerto Rico.
#'   Default: `"pr_geodata"`. Only needs to be changed if working with a
#'   different GeoServer instance.
#'
#' @param crs Integer. EPSG code to reproject data **before** visualizing
#'   (e.g. `32161` for Puerto Rico State Plane). If `NULL` (default), the
#'   original layer CRS is used. For display, the function always reprojects
#'   to WGS84 (EPSG:4326) internally.
#'
#' @param bbox Bounding box to limit the download area. Accepts:
#'   \itemize{
#'     \item An `sf::st_bbox()` object.
#'     \item A numeric vector of 4 elements: `c(xmin, ymin, xmax, ymax)`.
#'   }
#'   If `NULL` (default), downloads features from the full layer extent.
#'
#' @param cql_filter Character. CQL filter to select a subset of features by
#'   attribute. Uses OGC CQL standard syntax. Examples:
#'   \itemize{
#'     \item `"municipio = 'San Juan'"` -- features for San Juan only.
#'     \item `"area_km2 > 100"` -- features with area greater than 100 km2.
#'   }
#'   If `NULL` (default), no filter is applied.
#'
#' @param backend Character. Visualization engine. Options:
#'   \itemize{
#'     \item `"mapview"` (default): uses the `mapview` package, which provides
#'       a richer interface with automatic controls, scale bar, and legend.
#'       Requires `mapview` to be installed.
#'     \item `"leaflet"`: uses `leaflet` directly. Automatically activated as
#'       a fallback if `mapview` is not installed.
#'   }
#'   If neither is installed, the function throws an error with installation
#'   instructions.
#'
#' @param color Character. Main color for geometries on the map.
#'   Accepts hex colors (e.g. `"#E63946"`) or R color names
#'   (e.g. `"steelblue"`). Applied as fill for polygons and as the color for
#'   points and lines. Default: `"#E63946"` (coral red).
#'
#' @param alpha Numeric between `0` and `1`. Fill transparency level.
#'   `0` = fully transparent; `1` = fully opaque. Default: `0.4`.
#'   Only affects polygons and point circles.
#'
#' @param layer_name Character. Label displayed in the map legend.
#'   If `NULL` (default), derived automatically from the layer name
#'   (e.g. `"g03_municipios"`).
#'
#' @param use_cache Logical. If `TRUE` (default), reuses the cached layer list
#'   if it was already fetched in the current session, avoiding a new server
#'   call. Only applies when `layer` is numeric (\code{layer_id}).
#'
#' @param timeout Integer. Maximum wait time in seconds for feature download.
#'   Default: `60`. Increase this value if you have a slow connection or the
#'   layer is very large.
#'
#' @return Invisibly, the downloaded `sf` object (or the one passed as `layer`).
#'   The interactive map is rendered as a side effect in the RStudio
#'   **Viewer** panel or the default browser.
#'
#' @section Visualization backends:
#' The function tries to use `mapview` by default as it offers a more complete
#' experience (layer controls, automatic popups, multiple basemaps).
#' If unavailable, it falls back to `leaflet`. Install either with:
#' \preformatted{
#' install.packages("mapview")   # recommended
#' install.packages("leaflet")   # alternative
#' }
#'
#' @section Automatic tooltips:
#' When hovering over geometries, the map shows the value of the first
#' recognizable column (`nombre`, `name`, `municipio`, `descripcion`, `tipo`,
#' etc.). If none of those columns exist, the first non-geometry column is used.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{geopr_list_layers}} to discover available layers and their IDs.
#'   \item \code{\link{geopr_get_layer}} to download the full layer once identified.
#'   \item \code{\link{geopr_save}} to save the downloaded layer to disk.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Quick preview by short layer name
#' geopr_preview("g03_municipios")
#'
#' # 2. Preview with fewer features for faster loading
#' geopr_preview("g03_municipios", n = 78)  # PR has 78 municipalities
#'
#' # 3. Using the layer_id from geopr_list_layers()
#' layers <- geopr_list_layers()
#' layers  # identify the desired layer_id
#' geopr_preview(5)
#'
#' # 4. Pass an already-downloaded sf object directly
#' municipios <- geopr_get_layer("g03_municipios", all = TRUE)
#' geopr_preview(municipios, layer_name = "PR Municipalities")
#'
#' # 5. Change colors and transparency
#' geopr_preview("g03_municipios", color = "#2A9D8F", alpha = 0.6)
#'
#' # 6. Use leaflet as backend
#' geopr_preview("g03_municipios", backend = "leaflet")
#'
#' # 7. Filter by attribute with CQL before previewing
#' geopr_preview("g03_barrios", cql_filter = "municipio = 'San Juan'", n = 200)
#'
#' # 8. Limit by geographic area with bbox
#' geopr_preview("g03_barrios", bbox = c(-66.2, 18.3, -65.9, 18.5))
#'
#' # 9. Capture the returned sf for later use
#' sf_preview <- geopr_preview("g03_municipios", n = 20)
#' nrow(sf_preview)  # number of downloaded features
#' }
geopr_preview <- function(
    layer,
    n              = 500,
    wfs_url        = "http://geoserver2.pr.gov/geoserver/pr_geodata/wfs",
    serviceVersion = "2.0.0",
    namespace      = "pr_geodata",
    crs            = NULL,
    bbox           = NULL,
    cql_filter     = NULL,
    backend        = c("mapview", "leaflet"),
    color          = "#E63946",
    alpha          = 0.4,
    layer_name     = NULL,
    use_cache      = TRUE,
    timeout        = 60
) {
  # --- Validation ---
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install it with install.packages('sf').", call. = FALSE)
  }

  backend <- match.arg(backend)

  # --- Obtain the sf object ---
  if (inherits(layer, "sf")) {
    # Already an sf object: use it directly
    x <- layer
    resolved_name <- layer_name %||% "layer"
    message(sprintf("Previewing sf object: %s features.", nrow(x)))
  } else {
    # Resolve display name for the legend
    if (is.null(layer_name)) {
      resolved_name <- if (is.character(layer)) sub("^[^:]+:", "", layer) else paste0("layer_", layer)
    } else {
      resolved_name <- layer_name
    }

    message(sprintf("Downloading up to %s features from '%s' for preview...", n, resolved_name))

    x <- geopr_get_layer(
      layer          = layer,
      wfs_url        = wfs_url,
      serviceVersion = serviceVersion,
      namespace      = namespace,
      crs            = crs,
      bbox           = bbox,
      cql_filter     = cql_filter,
      max_features   = as.integer(n),
      start_index    = 0L,
      all            = FALSE,
      timeout        = timeout,
      use_cache      = use_cache
    )

    if (is.null(x) || nrow(x) == 0) {
      stop("The layer returned no features. Check the name or filters.", call. = FALSE)
    }

    message(sprintf("Downloaded %s features. Rendering map...", nrow(x)))
  }

  # --- Ensure valid CRS for visualization ---
  if (is.na(sf::st_crs(x))) {
    warning("The sf object has no CRS defined. Assigning WGS84 (EPSG:4326) for visualization.", call. = FALSE)
    sf::st_crs(x) <- 4326
  }

  # Reproject to WGS84 if needed for leaflet/mapview
  if (!is.na(sf::st_crs(x)) && sf::st_crs(x)$epsg != 4326) {
    x <- sf::st_transform(x, 4326)
  }

  # --- Detect geometry type ---
  geom_type <- unique(as.character(sf::st_geometry_type(x)))
  is_point   <- any(grepl("POINT",   geom_type, ignore.case = TRUE))
  is_line    <- any(grepl("LINE",    geom_type, ignore.case = TRUE))
  is_polygon <- any(grepl("POLYGON", geom_type, ignore.case = TRUE))

  # --- Render according to backend ---
  map_obj <- .preview_render(
    x          = x,
    backend    = backend,
    layer_name = resolved_name,
    color      = color,
    alpha      = alpha,
    is_point   = is_point,
    is_line    = is_line,
    is_polygon = is_polygon
  )

  print(map_obj)
  invisible(x)
}


# ---------------------------------------------------------------------------
# Internal helper: renders the map with mapview or leaflet
# ---------------------------------------------------------------------------
.preview_render <- function(x, backend, layer_name, color, alpha,
                             is_point, is_line, is_polygon) {

  # Try mapview first
  if (backend == "mapview") {
    if (requireNamespace("mapview", quietly = TRUE)) {
      return(
        mapview::mapview(
          x,
          layer.name  = layer_name,
          col.regions = color,
          alpha.regions = alpha,
          color       = color,
          legend      = TRUE
        )
      )
    } else {
      message("'mapview' is not installed. Falling back to 'leaflet'.")
      message("Install mapview with: install.packages('mapview')")
      backend <- "leaflet"
    }
  }

  # Leaflet as fallback
  if (backend == "leaflet") {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      stop(
        "No visualization backend is available.\n",
        "Install one with:\n  install.packages('mapview')  # recommended\n  install.packages('leaflet')",
        call. = FALSE
      )
    }

    m <- leaflet::leaflet() |>
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(noWrap = TRUE)
      )

    fill_color   <- color
    fill_opacity <- alpha
    stroke_color <- .darken_color(color, factor = 0.7)

    if (is_polygon) {
      m <- m |>
        leaflet::addPolygons(
          data         = x,
          fillColor    = fill_color,
          fillOpacity  = fill_opacity,
          color        = stroke_color,
          weight       = 1.2,
          opacity      = 0.9,
          highlightOptions = leaflet::highlightOptions(
            weight       = 2.5,
            color        = "#FFFFFF",
            fillOpacity  = 0.75,
            bringToFront = TRUE
          ),
          label = ~.make_label(x),
          group = layer_name
        )
    } else if (is_line) {
      m <- m |>
        leaflet::addPolylines(
          data    = x,
          color   = fill_color,
          weight  = 2,
          opacity = 0.85,
          label   = ~.make_label(x),
          group   = layer_name
        )
    } else if (is_point) {
      m <- m |>
        leaflet::addCircleMarkers(
          data         = x,
          radius       = 6,
          fillColor    = fill_color,
          fillOpacity  = fill_opacity,
          color        = stroke_color,
          weight       = 1,
          opacity      = 0.9,
          label        = ~.make_label(x),
          group        = layer_name
        )
    }

    # Legend and controls
    m <- m |>
      leaflet::addLayersControl(
        overlayGroups = layer_name,
        options       = leaflet::layersControlOptions(collapsed = FALSE)
      ) |>
      leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
      leaflet::addScaleBar(position = "bottomleft") |>
      .add_leaflet_title(layer_name, nrow(x))

    return(m)
  }
}


# ---------------------------------------------------------------------------
# Internal utility helpers
# ---------------------------------------------------------------------------

# Generates informative labels for popups/tooltips
.make_label <- function(x) {
  # Look for columns with common recognizable names
  candidates <- c("nombre", "name", "nom", "municipio", "descripcion",
                  "title", "tipo", "id", "objectid", "gid")
  col_names  <- tolower(names(x))
  col_names  <- col_names[col_names != attr(x, "sf_column")]

  match_col <- col_names[col_names %in% candidates]

  if (length(match_col) > 0) {
    col_real <- names(x)[tolower(names(x)) == match_col[1]]
    return(as.character(x[[col_real[1]]]))
  }

  # Fall back to the first non-geometry column
  non_geom <- names(sf::st_drop_geometry(x))
  if (length(non_geom) > 0) {
    return(as.character(x[[non_geom[1]]]))
  }

  return(rep("Feature", nrow(x)))
}

# Darkens a hex color for stroke/border use
.darken_color <- function(hex, factor = 0.7) {
  tryCatch({
    rgb_val <- grDevices::col2rgb(hex) / 255
    grDevices::rgb(rgb_val[1] * factor,
                   rgb_val[2] * factor,
                   rgb_val[3] * factor)
  }, error = function(e) "#333333")
}

# Adds a title overlay to a leaflet map
.add_leaflet_title <- function(map, title, n_features) {
  tag <- htmltools::tags$div(
    htmltools::HTML(
      sprintf(
        '<b style="font-size:13px">%s</b><br><span style="font-size:11px;color:#666">%s features</span>',
        title, format(n_features, big.mark = ",")
      )
    ),
    style = paste(
      "position:absolute; top:10px; right:10px; z-index:1000;",
      "background:rgba(255,255,255,0.85); padding:8px 12px;",
      "border-radius:6px; box-shadow:0 1px 5px rgba(0,0,0,0.25);",
      "pointer-events:none;"
    )
  )

  tryCatch(
    leaflet::addControl(map, html = tag, position = "topright"),
    error = function(e) map
  )
}

# Null-coalesce operator (defined here if not already present in the package)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b
}
