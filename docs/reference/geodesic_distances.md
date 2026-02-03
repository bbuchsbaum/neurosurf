# Geodesic distances from sources to targets

Convenience wrapper returning a numeric vector/matrix.

## Usage

``` r
geodesic_distances(
  geometry,
  sources,
  targets = NULL,
  weights = NULL,
  algorithm = c("dijkstra"),
  chunk_size = 2000
)
```

## Arguments

- geometry:

  `SurfaceGeometry`, `LabeledNeuroSurface`, `NeuroSurface`, or an
  `igraph`.

- sources:

  Integer vector of source vertices.

- targets:

  Optional target vertices (defaults to `sources`).

- weights:

  Optional numeric edge weights (defaults to `E(g)$dist`).

- algorithm:

  Only "dijkstra" is currently implemented.

- chunk_size:

  Number of source vertices per Dijkstra batch.

## Value

Numeric vector if one source, otherwise a matrix.

## Examples

``` r
geom <- example_surface_geometry()
d <- geodesic_distances(geom, sources = 1, targets = 2:4)
```
