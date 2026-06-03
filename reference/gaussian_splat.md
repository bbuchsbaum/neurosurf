# Gaussian splats on surface meshes

Create per-vertex Gaussian falloff maps centred on coordinates or
vertices. Distances can be measured either in straight-line Euclidean
space or along the mesh using geodesic paths (Dijkstra on edge lengths).

## Usage

``` r
gaussian_splat(surface, center, sigma, use_geodesic = FALSE)

gaussian_splat_vertex(surface, vertex_idx, sigma, use_geodesic = FALSE)

gaussian_splat_multi(
  surface,
  centers,
  sigmas,
  weights = NULL,
  use_geodesic = FALSE
)
```

## Arguments

- surface:

  A `SurfaceGeometry`.

- center:

  Numeric vector of length 3 giving the centre coordinate.

- sigma:

  Positive numeric standard deviation of the Gaussian kernel.

- use_geodesic:

  Logical; when `TRUE` use along-surface distances.

- vertex_idx:

  Integer index (1-based) of the centre vertex.

- centers:

  Numeric matrix with three columns; each row is a centre.

- sigmas:

  Numeric vector of length 1 or `nrow(centers)`.

- weights:

  Optional numeric vector of length 1 or `nrow(centers)` used to weight
  each centre (defaults to 1).

## Value

A `NeuroSurface` with values \\\exp(-d^2 / (2 \* sigma^2))\\ at every
vertex. For `gaussian_splat_multi` the per-centre maps are summed (after
weighting).

## Examples

``` r
geom <- example_surface_geometry()
g1 <- gaussian_splat(geom, center = c(0, 0, 0), sigma = 1)
v_idx <- find_nearest_vertex(geom, c(0.1, 0, 0))
g2 <- gaussian_splat_vertex(geom, vertex_idx = v_idx, sigma = 1)
g_multi <- gaussian_splat_multi(
  geom,
  centers = rbind(c(0, 0, 0), c(1, 0, 0)),
  sigmas = c(0.5, 1)
)
```
