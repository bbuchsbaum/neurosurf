# Parcel centroids using geodesic medoids

Parcel centroids using geodesic medoids

## Usage

``` r
parcel_geodesic_centroid(
  labeled_surface,
  method = c("medoid", "euclidean"),
  component_policy = c("error", "largest", "each", "merge"),
  weights = NULL,
  chunk_size = 2000,
  cache = TRUE
)
```

## Arguments

- labeled_surface:

  A `LabeledNeuroSurface`.

- method:

  `"medoid"` (geodesic mean distance) or `"euclidean"` (closest to
  Euclidean centroid).

- component_policy:

  How to handle fragmented parcels: `"error"`, `"largest"`, `"each"`,
  `"merge"`.

- weights:

  Optional numeric edge weights (defaults to `E(g)$dist`).

- chunk_size:

  Number of source vertices per Dijkstra batch.

- cache:

  Logical; cache symmetric results when `targets` is NULL or identical
  to `vertices`.

## Value

Data frame with one row per parcel unit: `unit`, `parcel_id`,
`component`, `size`, `centroid_vertex`, and Cartesian coordinates.

## Examples

``` r
# \donttest{
# Requires a LabeledNeuroSurface
# lsurf <- read_freesurfer_annot("lh.aparc.annot", geom)
# centroids <- parcel_geodesic_centroid(lsurf)
# }
```
