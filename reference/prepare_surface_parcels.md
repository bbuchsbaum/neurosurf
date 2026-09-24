# Prepare a labelled surface for parcel rendering

Precomputes the per-geometry quantities used by
\[render_surface_parcels()\]: outward vertex normals, a normalized
anatomical underlay, and smoothed label memberships that let parcel
boundaries follow smooth curves through mesh triangles instead of
stair-stepping along triangle edges. Reuse the result across cameras and
value maps.

## Usage

``` r
prepare_surface_parcels(
  geometry,
  labels,
  anatomy_metric = NULL,
  cortex_mask = NULL,
  boundary_smooth = 3L
)
```

## Arguments

- geometry:

  Display \`SurfaceGeometry\` (typically inflated).

- labels:

  Integer parcel label per vertex; 0 marks the medial wall.

- anatomy_metric:

  Optional sulcal-depth or curvature value per vertex, larger on gyri.
  \`NULL\` gives a neutral underlay.

- cortex_mask:

  Optional logical per vertex; \`FALSE\` vertices are treated as medial
  wall. Defaults to \`labels != 0\`.

- boundary_smooth:

  Laplacian iterations applied to label memberships. 0 gives
  piecewise-linear boundaries through triangle edge midpoints.

## Value

A \`surface_parcel_prep\` object.
