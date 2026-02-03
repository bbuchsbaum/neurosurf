# All-pairs geodesic distance matrix (chunked)

All-pairs geodesic distance matrix (chunked)

## Usage

``` r
geodesic_distance_matrix(
  geometry,
  vertices = NULL,
  targets = NULL,
  weights = NULL,
  mode = c("sparse", "dense"),
  chunk_size = 2000,
  cache = TRUE,
  cache_key = NULL,
  algorithm = c("dijkstra")
)
```

## Arguments

- geometry:

  `SurfaceGeometry`, `LabeledNeuroSurface`, `NeuroSurface`, or an
  `igraph`.

- vertices:

  Optional integer vector of source vertices. Defaults to all vertices.

- targets:

  Optional integer vector of target vertices. Defaults to `vertices`
  (square matrix).

- weights:

  Optional numeric edge weights (defaults to `E(g)$dist`).

- mode:

  Output mode: `"sparse"` (default, `dgCMatrix`) or `"dense"`.

- chunk_size:

  Number of source vertices per Dijkstra batch.

- cache:

  Logical; cache symmetric results when `targets` is NULL or identical
  to `vertices`.

- cache_key:

  Optional manual cache key.

- algorithm:

  Only "dijkstra" is currently implemented.

## Value

A distance matrix (sparse or dense) of size
`length(vertices) x length(targets)`.

## Examples

``` r
geom <- example_surface_geometry()
dmat <- geodesic_distance_matrix(geom, vertices = 1:2)
```
