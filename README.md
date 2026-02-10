
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geopr

<!-- badges: start -->
<!-- badges: end -->

The goal of GeoPR is to provide a simple and reliable interface for
accessing official geospatial data from the Puerto Rico government
GeoServer through its WFS service.

GeoPR allows users to:

- Discover available geospatial layers published by the Puerto Rico
  GeoServer.

- Download selected layers directly into R as sf objects, ready for
  analysis and mapping.

- Work with layers using stable numeric identifiers or layer names,
  without needing to manually construct WFS queries.

The package is designed primarily for researchers, analysts, and
practitioners working with geographic, demographic, environmental, and
planning data related to Puerto Rico. GeoPR abstracts away the technical
complexity of WFS requests while preserving transparency and flexibility
for advanced users.

This package is currently experimental.

## Installation

You can install the development version of geopr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("lanselotteoliveras/GeoPR", dependencies = TRUE)
```

## Basic usage

### List available layers

``` r
library(geopr)

layers <- geopr_list_layers()
print(capas)
```

This returns a tidy table with:

- a stable numeric `layer_id`,

- layer name,

- thematic group,

- theme and subtheme,

- year (when available).

### Download a layer

Layers can be downloaded using either the numeric identifier or the
layer name.

``` r
# Using layer name
municipios <- geopr_get_layer("g03_legales_municipios_2023")

# Using numeric layer_id
municipios <- geopr_get_layer(1)
```

All layers are returned as sf objects.

### Download a complete layer (automatic pagination)

Some layers may exceed the default WFS page size. To retrieve the full
layer, use:

This ensures that no features are silently truncated.

## Data source

GeoPR accesses data from the official Puerto Rico government GeoServer
WFS endpoint: <http://geoserver2.pr.gov/geoserver/pr_geodata/wfs>

All data ownership, maintenance, and publication remain with the
original data providers.

## Experimental status

GeoPR is under active development.

- The functions may change.

- Not all layers have been fully tested.

- Error handling and performance optimizations are ongoing.

Feedback, bug reports, and suggestions are welcome via GitHub Issues or
vis bsky chat.
