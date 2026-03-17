#' Download a layer from the Puerto Rico WFS as an sf object (by id or name)
#'
#' @param layer Layer identifier: numeric (layer_id) or character.
#'   - "pr_geodata:g03_..." (full name) or
#'   - "g03_..." (short layer name)
#' @param wfs_url WFS service URL.
#' @param serviceVersion WFS version.
#' @param namespace Default namespace if `layer` is provided without one (default "pr_geodata").
#' @param crs EPSG code (optional).
#' @param bbox Optional bounding box: sf::st_bbox() or numeric c(xmin,ymin,xmax,ymax).
#' @param cql_filter Optional CQL filter string.
#' @param max_features Page size (count) per request. Default 5000.
#' @param start_index Initial startIndex (useful to resume manually). Default 0.
#' @param all If TRUE, automatically paginates until all features are retrieved. Default FALSE.
#' @param timeout Request timeout in seconds.
#' @param use_cache Reuse cached layer list from geopr_list_layers().
#'
#' @return An `sf` object.
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
    stop("Package 'sf' is required. Install it with install.packages('sf').", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install it with install.packages('httr2').", call. = FALSE)
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

  # 1) Resolve correct typeNames
  typeNames <- NULL

  if (is.numeric(layer) && length(layer) == 1) {
    layers_df <- geopr_list_layers(
      wfs_url = wfs_url,
      serviceVersion = serviceVersion,
      use_cache = use_cache
    )
    id <- as.integer(layer)
    layer_short <- layers_df$layer[layers_df$layer_id == id]
    if (length(layer_short) != 1 || is.na(layer_short)) {
      stop("Could not find `layer_id` = ", id, " in geopr_list_layers().", call. = FALSE)
    }
    typeNames <- paste0(namespace, ":", layer_short)

  } else if (is.character(layer) && length(layer) == 1 && nzchar(layer)) {
    # Already includes namespace (e.g. pr_geodata:g03_...)
    if (grepl("^[^:]+:.+", layer)) {
      typeNames <- layer
    } else {
      typeNames <- paste0(namespace, ":", layer)
    }

  } else {
    stop("`layer` must be numeric (layer_id) or character (full name or short layer name).", call. = FALSE)
  }

  # 2) Normalize bbox if provided
  bbox_param <- NULL
  if (!is.null(bbox)) {
    bb <- NULL
    if (inherits(bbox, "bbox")) {
      bb <- bbox
    } else if (is.numeric(bbox) && length(bbox) == 4) {
      bb <- c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4])
      class(bb) <- "bbox"
    } else {
      stop("`bbox` must be an sf::st_bbox() object or a numeric vector c(xmin,ymin,xmax,ymax).", call. = FALSE)
    }
    bbox_param <- paste(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]], sep = ",")
  }

  # Internal helper: fetch one page (not exported)
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
        sprintf("GetFeature failed (HTTP %s) for %s (startIndex=%s).", status, typeNames, start_idx),
        if (nzchar(body)) paste0("\nResponse (first 2000 chars):\n", substr(body, 1, 2000)) else "",
        call. = FALSE
      )
    }

    geojson_txt <- httr2::resp_body_string(resp, encoding = "UTF-8")
    sf::st_read(geojson_txt, quiet = TRUE)
  }

  # 3) all=FALSE: single page
  if (!isTRUE(all)) {
    return(fetch_page(start_index))
  }

  # 4) all=TRUE: paginate automatically
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

    # if fewer features than max_features were returned, we have reached the end
    if (n < as.integer(max_features)) break
  }

  if (length(out_list) == 1) return(out_list[[1]])
  do.call(rbind, out_list)

  }, error = function(e) {
    stop(conditionMessage(e), .server_error_msg, call. = FALSE)
  })
}
