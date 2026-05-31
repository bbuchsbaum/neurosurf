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
  boundary_method = c("midpoint", "faces", "edge_vertices"),
  verbose = FALSE,
  use_cpp = TRUE
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

  One of `"midpoint"`, `"faces"`, or `"edge_vertices"`.

- verbose:

  Logical; if `TRUE`, print progress messages.

- use_cpp:

  Logical; if `TRUE`, use optimized C++ implementation (applies to
  `"edge_vertices"` only).

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

## Details

Three boundary representations are currently supported:

- `"midpoint"` (default): returns crisp single-width contour segments
  that run *between* differing labels, through the midpoints of the mesh
  edges that separate them. This is the recommended method for drawing
  clean ROI/atlas outlines: shared borders are drawn once (no double
  lines) and adjacent segments join into continuous contours.

- `"faces"`: returns a logical vector indicating which faces lie on a
  boundary between ROIs.

- `"edge_vertices"`: returns boundary polygons traced through mesh
  vertices. Borders are traced along vertices on both sides of a
  boundary, which can read as a thicker double line on coarse meshes.

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
