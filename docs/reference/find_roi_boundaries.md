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
  boundary_method = c("faces", "edge_vertices")
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

  One of `"faces"` or `"edge_vertices"`.

## Value

A list with elements:

- `boundary`:

  For `"faces"`: logical vector of length `nrow(faces)` indicating
  boundary faces. For `"edge_vertices"`: list of coordinate matrices
  (\\k \times 3\\) giving boundary polygons for each connected ROI
  component.

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

Two boundary representations are currently supported:

- `"faces"`: returns a logical vector indicating which faces lie on a
  boundary between ROIs.

- `"edge_vertices"`: returns boundary polygons traced through mesh
  vertices, suitable for drawing ROI outlines.

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
#> Error in igraph::get.edge.ids(g, c(start_node, end_node)): Only two-column data.frames and matrices, and vectors are allowed for vp
str(b$boundary)
#> Error: object 'b' not found
# }
```
