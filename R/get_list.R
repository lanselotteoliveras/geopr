#' List available layers from the Puerto Rico WFS (GeoServer)
#'
#' This function queries the Web Feature Service (WFS) endpoint of the
#' Puerto Rico GeoServer and returns a list of available layers along with
#' essential metadata for identification and downstream use.
#'
#' @details
#' The function performs a \code{GetCapabilities} request to the specified
#' WFS endpoint and processes the XML response to extract only the most
#' relevant information for initial service exploration.
#'
#' The output is intentionally simplified to be human-readable and easy to
#' filter, avoiding deeply nested or overly technical structures from the
#' original XML. This function is typically used as a first step before
#' downloading a specific layer using \code{geopr_get_layer()}.
#'
#' @param wfs_url Base URL of the WFS service. By default, it points to the
#' public Puerto Rico GeoServer.
#'
#' @return
#' A \code{tibble} with one row per available layer in the WFS service.
#' The output includes the following columns:
#' \describe{
#'   \item{layer_name}{Technical layer name as published in GeoServer.}
#'   \item{title}{Human-readable title describing the layer.}
#'   \item{crs}{Declared coordinate reference system (EPSG).}
#' }
#'
#' @section Design:
#' The function returns a deliberately reduced subset of the available
#' metadata in order to support quick exploration and reduce cognitive
#' overhead. More advanced metadata may be exposed in future versions.
#'
#' @note
#' Layer availability and metadata depend on the upstream GeoServer service.
#' Changes or outages in the service may affect the function output.
#'
#' @seealso
#' \code{\link{geopr_get_layer}} to download a specific layer from the service.
#'
#' @examples
#' \dontrun{
#' layers <- geopr_list_layers()
#' head(layers)
#'
#' layers |>
#'   dplyr::filter(grepl("municipio", layer_name, ignore.case = TRUE))
#' }
#'
#' @export

# Ambiente interno para cache (en tu paquete esto iría en un archivo zzz.R)
.geopr_cache <- new.env(parent = emptyenv())

#' Listar capas disponibles del WFS de Puerto Rico (GeoServer PR) con metadatos
#'
#' @param wfs_url URL del servicio WFS.
#' @param serviceVersion Versión WFS.
#' @param use_cache Si TRUE, reutiliza el resultado en memoria durante la sesión.
#' @export
geopr_list_layers <- function(
    wfs_url = "http://geoserver2.pr.gov/geoserver/pr_geodata/wfs",
    serviceVersion = "2.0.0",
    use_cache = TRUE
) {
  if (!requireNamespace("ows4R", quietly = TRUE)) {
    stop("Falta 'ows4R'. Instala con install.packages('ows4R').", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Falta 'tibble'. Instala con install.packages('tibble').", call. = FALSE)
  }

  cache_key <- paste0("layers|", wfs_url, "|", serviceVersion)
  if (isTRUE(use_cache) && exists(cache_key, envir = .geopr_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .geopr_cache, inherits = FALSE))
  }

  client <- ows4R::WFSClient$new(wfs_url, serviceVersion = serviceVersion)
  ft <- client$getFeatureTypes(pretty = TRUE)

  # 'name' es lo único indispensable; con eso derivamos todo
  nm <- as.character(ft[["name"]])
  layer <- sub("^[^:]+:", "", nm)

  # Orden estable para layer_id
  o <- order(nm)
  nm <- nm[o]
  layer <- layer[o]

  # grupo gXX
  grupo <- regmatches(layer, regexpr("^g[0-9]{2}", layer))

  # year YYYY al final (si existe)
  m_year <- regexpr("_[0-9]{4}$", layer)
  year <- ifelse(m_year > 0, as.integer(sub("^_", "", regmatches(layer, m_year))), NA_integer_)

  # core: quitar "gXX_" al inicio y "_YYYY" al final
  core <- sub("^g[0-9]{2}_", "", layer)
  core <- sub("_[0-9]{4}$", "", core)

  # tema: hasta el primer "_"
  tema <- sub("_.*$", "", core)

  # subtema: lo que queda luego de "tema_"
  subtema <- sub("^[^_]+_?", "", core)
  subtema[subtema == "" | subtema == core] <- NA_character_

  out <- tibble::tibble(
    layer_id = seq_along(nm),
    layer = layer,
    grupo = grupo,
    tema = tema,
    subtema = subtema,
    year = year
  )

  if (isTRUE(use_cache)) {
    assign(cache_key, out, envir = .geopr_cache)
  }

  out
}
