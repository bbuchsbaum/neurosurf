# ROISurface

A class that respresents a surface-based region of interest

## Value

An object of class `ROISurface`.

## Details

The ROISurface class provides a way to represent a specific subset of
vertices on a brain surface along with their associated data values.
This is particularly useful for analyzing or visualizing specific
anatomical or functional regions on the cortical surface.

The class maintains a reference to the complete parent surface geometry
while storing only the relevant subset of vertices, their coordinates,
and their data values. The `indices` slot allows mapping back to the
original vertex indices in the parent surface.

Typical use cases include:

- Extracting and analyzing data from anatomical regions of interest

- Working with functional clusters identified in analyses

- Isolating specific surface features for detailed investigation

- Statistical analysis of data within defined surface regions

## Slots

- `geometry`:

  the geometry of the parent surface: a `SurfaceGeometry` instance

- `data`:

  the vector-valued `numeric` data stored in ROI

- `coords`:

  the surface-based coordinates of the data

- `indices`:

  the node indices of the parent surface stored in the `geometry` field.

## Examples

``` r
# \donttest{
# Create a simple tetrahedron mesh for the example
vertices <- c(
  0, 0, 0,
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
)
triangles <- c(
  1, 2, 3,
  1, 2, 4,
  1, 3, 4,
  2, 3, 4
)

# Create mesh3d object
mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)

# Create a graph representation
edges <- rbind(
  c(1,2), c(1,3), c(1,4),
  c(2,3), c(2,4),
  c(3,4)
)
graph <- igraph::graph_from_edgelist(edges)

# Create a SurfaceGeometry object
geometry <- new("SurfaceGeometry",
               mesh = mesh,
               graph = graph,
               hemi = "left")

# Define the ROI - just using vertices 1 and 2 as an example
roi_indices <- c(1L, 2L)

# Extract coordinates for these vertices
roi_coords <- matrix(
  c(0, 0, 0,  # coordinates for vertex 1
    1, 0, 0), # coordinates for vertex 2
  ncol = 3, byrow = TRUE
)

# Create data values for the ROI vertices
roi_data <- c(0.75, 1.25)  # example values for the two vertices

# Create the ROISurface object
roi <- new("ROISurface",
          geometry = geometry,
          data = roi_data,
          coords = roi_coords,
          indices = roi_indices)
# }
```
