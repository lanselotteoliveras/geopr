#' Descargar una capa del WFS de Puerto Rico como sf (por id o nombre)
#'
#' @param layer Identificador: numeric (layer_id) o character
#'   - "pr_geodata:g03_..." (name completo) o
#'   - "g03_..." (layer corto)
#' @param wfs_url URL del WFS.
#' @param serviceVersion Versión WFS.
#' @param namespace Namespace por defecto si `layer` viene corto (default "pr_geodata").
#' @param crs EPSG destino (opcional).
#' @param bbox Bounding box opcional: sf::st_bbox() o numeric c(xmin,ymin,xmax,ymax)
#' @param cql_filter Filtro CQL opcional.
#' @param max_features Tamaño de página (count) por request. Default 5000.
#' @param start_index startIndex inicial (útil si quieres continuar manualmente). Default 0.
#' @param all Si TRUE, pagina automáticamente hasta traer todo. Default FALSE.
#' @param timeout Timeout.
#' @param use_cache Reutiliza cache de geopr_list_layers().
#'
#' @return Objeto `sf`.
#' @export
geopr_get_layer <- function(
    layer,
    wfs_url = "http://geoserver2.pr.gov/geoserver/pr_geodata/wfs",
    serviceVersion = "2.0.0",
    namespace = "pr_geodata",
    crs = NULL,
    bbox = NULL,
    cql_filter = NULL,
    max_features = 5000,
    start_index = 0,
    all = FALSE,
    timeout = 120,
    use_cache = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Falta 'sf'. Instala con install.packages('sf').", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Falta 'httr2'. Instala con install.packages('httr2').", call. = FALSE)
  }

  # 1) Resolver typeNames correcto
  typeNames <- NULL

  if (is.numeric(layer) && length(layer) == 1) {
    capas <- geopr_list_layers(
      wfs_url = wfs_url,
      serviceVersion = serviceVersion,
      use_cache = use_cache
    )
    id <- as.integer(layer)
    layer_short <- capas$layer[capas$layer_id == id]
    if (length(layer_short) != 1 || is.na(layer_short)) {
      stop("No encontré `layer_id` = ", id, " en geopr_list_layers().", call. = FALSE)
    }
    typeNames <- paste0(namespace, ":", layer_short)

  } else if (is.character(layer) && length(layer) == 1 && nzchar(layer)) {
    # Si ya viene con namespace (ej. pr_geodata:g03_...)
    if (grepl("^[^:]+:.+", layer)) {
      typeNames <- layer
    } else {
      typeNames <- paste0(namespace, ":", layer)
    }

  } else {
    stop("`layer` debe ser numeric (layer_id) o character (name completo o layer corto).", call. = FALSE)
  }

  # 2) Normalizar bbox si aplica
  bbox_param <- NULL
  if (!is.null(bbox)) {
    bb <- NULL
    if (inherits(bbox, "bbox")) {
      bb <- bbox
    } else if (is.numeric(bbox) && length(bbox) == 4) {
      bb <- c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4])
      class(bb) <- "bbox"
    } else {
      stop("`bbox` debe ser sf::st_bbox() o numeric c(xmin,ymin,xmax,ymax).", call. = FALSE)
    }
    bbox_param <- paste(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]], sep = ",")
  }

  # Helper interno: trae una página (NO exportado)
  fetch_page <- function(start_idx) {
    req <- httr2::request(wfs_url)
    req <- httr2::req_url_query(
      req,
      service = "WFS",
      request = "GetFeature",
      version = serviceVersion,
      typeNames = typeNames,
      outputFormat = "application/json",
      count = as.integer(max_features),
      startIndex = as.integer(start_idx)
    )
    req <- httr2::req_timeout(req, timeout)
    req <- httr2::req_headers(
      req,
      `Accept` = "application/json,*/*",
      `User-Agent` = "GeoPR (R; httr2)"
    )

    if (!is.null(bbox_param)) req <- httr2::req_url_query(req, bbox = bbox_param)
    if (!is.null(cql_filter) && nzchar(cql_filter)) req <- httr2::req_url_query(req, cql_filter = cql_filter)
    if (!is.null(crs)) req <- httr2::req_url_query(req, srsName = paste0("EPSG:", as.integer(crs)))

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)

    if (status >= 400) {
      body <- tryCatch(httr2::resp_body_string(resp, encoding = "UTF-8"), error = function(e) "")
      stop(
        sprintf("GetFeature falló (HTTP %s) para %s (startIndex=%s).", status, typeNames, start_idx),
        if (nzchar(body)) paste0("\nRespuesta (primeros 2000 chars):\n", substr(body, 1, 2000)) else "",
        call. = FALSE
      )
    }

    geojson_txt <- httr2::resp_body_string(resp, encoding = "UTF-8")
    sf::st_read(geojson_txt, quiet = TRUE)
  }

  # 3) all=FALSE: una sola página
  if (!isTRUE(all)) {
    return(fetch_page(start_index))
  }

  # 4) all=TRUE: paginar automáticamente
  out_list <- list()
  idx <- as.integer(start_index)
  k <- 1L

  repeat {
    page <- fetch_page(idx)
    n <- nrow(page)

    if (is.null(n) || n == 0) break

    out_list[[k]] <- page
    k <- k + 1L

    idx <- idx + n

    # si llegó menos que max_features, ya no hay más
    if (n < as.integer(max_features)) break
  }

  if (length(out_list) == 1) return(out_list[[1]])
  do.call(rbind, out_list)
}
