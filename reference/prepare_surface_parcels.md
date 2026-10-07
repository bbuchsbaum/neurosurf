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
  boundary_smooth = 3L,
  boundary_method = c("membership", "smooth"),
  t_clamp = c(0.1, 0.9)
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

- boundary_method:

  `"membership"` (default) retains the original per-face
  interpolated-membership argmax. `"smooth"` prepares the shared
  vertex-preserving partition used by \[find_roi_boundaries()\].

- t_clamp:

  Symmetric edge bounds for `"smooth"`; see \[find_roi_boundaries()\].
  Ignored by `"membership"`.

## Value

A \`surface_parcel_prep\` object.

## Details

In smooth mode, labels must be nonnegative integers. The cortex mask is
applied before partition construction; preservation refers to these
effective labels. The prepared object retains labelled face cells,
boundary paths and their provenance in `partition`. Rendering reuses
this partition across cameras and value maps. The partition does not
reproduce the unconstrained membership argmax. Existing input behaviour
is retained in membership mode, except that fractional smoothing counts
are rejected rather than truncated.
