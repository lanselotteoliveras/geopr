#' List available layers from the Puerto Rico WFS (GeoServer)
#'
#' Queries the Web Feature Service (WFS) endpoint of the Puerto Rico GeoServer
#' and returns a tidy table of available layers with essential metadata for
#' identification and downstream use.
#'
#' @details
#' The function performs a \code{GetCapabilities} request to the specified
#' WFS endpoint and processes the XML response to extract only the most
#' relevant information for initial service exploration.
#'
#' The output is intentionally simplified to be human-readable and easy to
#' filter, avoiding deeply nested or overly technical structures from the
#' original XML. This function is typically used as a first step before
#' downloading a specific layer with \code{\link{geopr_get_layer}}.
#'
#' @param wfs_url Character. Base URL of the WFS service. Defaults to the
#'   public Puerto Rico GeoServer endpoint.
#' @param serviceVersion Character. WFS protocol version. Default \code{"2.0.0"}.
#' @param use_cache Logical. If \code{TRUE} (default), reuses the cached result
#'   in memory during the current R session, avoiding repeated server calls.
#'
#' @return
#' A \code{tibble} with one row per available layer. Columns:
#' \describe{
#'   \item{layer_id}{Stable numeric identifier assigned by geopr.}
#'   \item{layer}{Short layer name (without namespace prefix).}
#'   \item{grupo}{Layer group code (e.g. \code{"g03"}).}
#'   \item{tema}{Primary theme extracted from the layer name.}
#'   \item{subtema}{Secondary theme extracted from the layer name (when present).}
#'   \item{year}{Year embedded in the layer name (when present).}
#' }
#'
#' @section Design:
#' The function returns a deliberately reduced subset of the available
#' metadata to support quick exploration and reduce cognitive overhead.
#' More advanced metadata may be exposed in future versions.
#'
#' @note
#' Layer availability and metadata depend on the upstream GeoServer service.
#' Changes or outages in the service may affect the function output.
#'
#' @seealso
#' \code{\link{geopr_get_layer}} to download a specific layer.
#'
#' @examples
#' \dontrun{
#' layers <- geopr_list_layers()
#' head(layers)
#'
#' # Search layers by keyword
#' layers[grepl("municipio", layers$layer, ignore.case = TRUE), ]
#' }
#'
#' @export
geopr_list_layers <- function(
    wfs_url = "http://geoserver2.pr.gov/geoserver/pr_geodata/wfs",
    serviceVersion = "2.0.0",
    use_cache = TRUE
) {
  if (!requireNamespace("ows4R", quietly = TRUE)) {
    stop("Package 'ows4R' is required. Install it with install.packages('ows4R').", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Install it with install.packages('tibble').", call. = FALSE)
  }

  .server_error_msg <- paste0(
    "\n\n--- SERVICE NOTICE ---\n",
    "This error may be caused by a temporary issue with the gis.pr.gov server.\n",
    "Please try again later.\n\n",
    "If the error persists, send an email to:\n",
    "  support@prits.pr.gov\n",
    "with the following message:\n",
    "  \"This is to notify that the WFS service at gis.pr.gov is down\"\n",
    "----------------------"
  )

  tryCatch({

  cache_key <- paste0("layers|", wfs_url, "|", serviceVersion)
  if (isTRUE(use_cache) && exists(cache_key, envir = .geopr_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .geopr_cache, inherits = FALSE))
  }

  client <- ows4R::WFSClient$new(wfs_url, serviceVersion = serviceVersion)
  ft <- client$getFeatureTypes(pretty = TRUE)

  # 'name' is the only required field; everything else is derived from it
  nm <- as.character(ft[["name"]])
  layer <- sub("^[^:]+:", "", nm)

  # Stable ordering for layer_id
  o <- order(nm)
  nm <- nm[o]
  layer <- layer[o]

  # group code gXX
  grupo <- regmatches(layer, regexpr("^g[0-9]{2}", layer))

  # year YYYY at the end (if present)
  m_year <- regexpr("_[0-9]{4}$", layer)
  year <- ifelse(m_year > 0, as.integer(sub("^_", "", regmatches(layer, m_year))), NA_integer_)

  # core: strip "gXX_" prefix and "_YYYY" suffix
  core <- sub("^g[0-9]{2}_", "", layer)
  core <- sub("_[0-9]{4}$", "", core)

  # tema: up to the first "_"
  tema <- sub("_.*$", "", core)

  # subtema: everything after "tema_"
  subtema <- sub("^[^_]+_?", "", core)
  subtema[subtema == "" | subtema == core] <- NA_character_

  out <- tibble::tibble(
    layer_id = seq_along(nm),
    layer    = layer,
    grupo    = grupo,
    tema     = tema,
    subtema  = subtema,
    year     = year
  )

  if (isTRUE(use_cache)) {
    assign(cache_key, out, envir = .geopr_cache)
  }

  out

  }, error = function(e) {
    stop(conditionMessage(e), .server_error_msg, call. = FALSE)
  })
}
