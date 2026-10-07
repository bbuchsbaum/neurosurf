# Find boundaries of ROIs on a surface mesh

This function identifies boundaries between regions of interest (ROIs)
defined on a triangular surface mesh. It is a translation of the core
parts of Stuart Oldham's `findROIboundaries` MATLAB function, adapted
for use with neurosurf objects.

## Usage

``` r
find_roi_boundaries(
  vertices,
  faces,
  vertex_id,
  boundary_method = c("midpoint", "faces", "edge_vertices", "smooth"),
  verbose = FALSE,
  use_cpp = TRUE,
  boundary_smooth = 3L,
  t_clamp = c(0.1, 0.9)
)
```

## Arguments

- vertices:

  Numeric matrix of vertex coordinates (\\n \times 3\\).

- faces:

  Integer matrix of face indices (\\m \times 3\\, 1-based).

- vertex_id:

  Integer vector of ROI labels for each vertex (length \\n\\).

- boundary_method:

  One of `"midpoint"`, `"faces"`, `"edge_vertices"`, or `"smooth"`.

- verbose:

  Logical; if `TRUE`, print progress messages.

- use_cpp:

  Logical; if `TRUE`, use optimized C++ implementation (applies to
  `"edge_vertices"` only).

- boundary_smooth:

  Non-negative integer membership-smoothing iterations for `"smooth"`.
  Zero reproduces midpoint segment geometry.

- t_clamp:

  For `"smooth"`, symmetric edge-fraction bounds `c(lo, 1-lo)`, with
  `0 < lo <= 1/3` and upper bound below 1. `lo` also bounds each
  junction barycentric coordinate. Cannot be NULL.

## Value

A list with elements:

- `boundary`:

  For `"faces"`: logical vector of length `nrow(faces)` indicating
  boundary faces. For `"midpoint"`: list of \\2 \times 3\\ coordinate
  matrices, one per contour segment. For `"edge_vertices"`: list of
  coordinate matrices (\\k \times 3\\) giving boundary polygons for each
  connected ROI component.

- `boundary_roi_id`:

  Integer vector giving the ROI id associated with each boundary polygon
  (empty for `"faces"`).

- `roi_components`:

  Integer vector giving the number of connected components for each ROI
  (indexed parallel to `sort(unique(vertex_id))`).

- `boundary_verts`:

  For `"edge_vertices"`: list of integer vectors giving the vertex ids
  used for each boundary polygon; `NULL` for `"faces"`.

For `"smooth"`, `boundary` contains ordered polylines (closed paths
repeat their first point). Additional fields are `label_pair` (two
sorted labels per path), `closed`, `path_nodes`, a unique `nodes` table,
and `crossings`/`junctions` provenance tables. Crossings store canonical
vertex indices `i < j`, endpoint labels, `node_id`, signed margins
`d_i`/`d_j`, pre-clamp `t_raw`, final `t`, and fallback/clamp flags.
Junctions store `face_id`, `node_id`, raw `w_raw1:w_raw3` and final
`w1:w3` barycentrics in input-face order, and fallback/projection flags.
`provenance` records the algorithm version, kernel, iterations, clamp,
score tolerance, and tie rule (smallest incident label). Legacy
single-owner `boundary_roi_id`, `roi_components`, and `boundary_verts`
are NULL in this mode. Empty results preserve field types and
dimensions.

## Details

Four boundary representations are currently supported:

- `"midpoint"` (default): returns crisp single-width contour segments
  that run *between* differing labels, through the midpoints of the mesh
  edges that separate them. This is the recommended method for drawing
  clean ROI/atlas outlines: shared borders are drawn once (no double
  lines) and adjacent segments join into continuous contours.

- `"smooth"`: returns chained polylines from a constrained partition
  guided by smoothed memberships. Original vertex labels and mesh-edge
  parcel adjacency are preserved, including small parcels.

- `"faces"`: returns a logical vector indicating which faces lie on a
  boundary between ROIs.

- `"edge_vertices"`: returns boundary polygons traced through mesh
  vertices. Borders are traced along vertices on both sides of a
  boundary, which can read as a thicker double line on coarse meshes.

The smooth mode uses distinct one-ring neighbours and a lazy mean with
weight 0.5. It places crossings using nonnegative endpoint label
margins, falling back to a midpoint when neither endpoint supports its
original label. Three-label junctions are equal-score solutions
projected onto the interior barycentric simplex; singular solutions use
the centroid. This is a piecewise-linear, vertex-preserving partition,
not exact extraction of the unconstrained membership argmax. It matches
`prepare_surface_parcels(boundary_method = "smooth")` for the same
geometry, effective labels, smoothing, and clamp.

Smooth labels must be nonnegative R-representable integers (0 denotes
the medial wall in rendering). Open meshes and unused vertices are
supported. Duplicate/zero-area faces, repeated face indices, and edges
incident to more than two faces are rejected. A non-manifold vertex
alone is allowed. Shared border nodes are keyed by topology, never
rounded coordinates. Existing modes retain their original input and
output contracts.

## Examples

``` r
# \donttest{
# Simple cube mesh with two ROIs (bottom vs top)
vertices <- matrix(c(
  0,0,0, 1,0,0, 1,1,0, 0,1,0,
  0,0,1, 1,0,1, 1,1,1, 0,1,1
), ncol = 3, byrow = TRUE)

faces <- matrix(c(
  1,2,3, 1,3,4,
  5,6,7, 5,7,8,
  1,2,6, 1,6,5,
  3,4,8, 3,8,7,
  1,4,8, 1,8,5,
  2,3,7, 2,7,6
), ncol = 3, byrow = TRUE)

roi <- c(1,1,1,1, 2,2,2,2)

b <- find_roi_boundaries(vertices, faces, roi, boundary_method = "edge_vertices")
str(b$boundary)
#> List of 2
#>  $ : num [1:5, 1:3] 0 1 1 0 0 0 0 1 1 0 ...
#>  $ : num [1:5, 1:3] 0 1 1 0 0 0 0 1 1 0 ...
# }
```
