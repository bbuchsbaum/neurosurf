# Parcel-to-parcel geodesic distances

Parcel-to-parcel geodesic distances

## Usage

``` r
parcel_geodesic_distance_matrix(
  labeled_surface,
  metric = c("centroid", "min"),
  component_policy = c("error", "largest", "each", "merge"),
  weights = NULL,
  chunk_size = 2000,
  cache = TRUE
)
```

## Arguments

- labeled_surface:

  A `LabeledNeuroSurface`.

- metric:

  `"centroid"` (distance between parcel medoids) or `"min"` (minimum
  distance between any vertices of two parcels).

- component_policy:

  Fragment handling policy.

- weights:

  Optional numeric edge weights (defaults to `E(g)$dist`).

- chunk_size:

  Number of source vertices per Dijkstra batch.

- cache:

  Logical; cache symmetric results when `targets` is NULL or identical
  to `vertices`.

## Value

A numeric matrix with parcel-unit names as dimnames.

## Examples

``` r
# \donttest{
# Requires a LabeledNeuroSurface
# lsurf <- read_freesurfer_annot("lh.aparc.annot", geom)
# dmat <- parcel_geodesic_distance_matrix(lsurf)
# }
```
