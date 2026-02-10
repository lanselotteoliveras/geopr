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
