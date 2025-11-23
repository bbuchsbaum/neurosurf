# NeuroSurfaceVector Class

Represents a 3D surface with multiple values per vertex.

## Details

The NeuroSurfaceVector class extends the concept of NeuroSurface to
handle multiple measurements for each vertex across the entire surface.
Unlike NeuroSurface which stores a single value per vertex,
NeuroSurfaceVector stores a matrix of values where columns represent
different measures and rows correspond to vertices.

This structure is particularly useful for:

- Time series data where each column represents a different timepoint

- Multi-modal data where each column represents a different imaging
  modality

- Statistical results where columns represent different statistical
  parameters

- Feature vectors for machine learning applications

The data matrix organization (vertices as rows, measures as columns)
facilitates efficient vertex-wise operations and analyses. This is in
contrast to ROISurfaceVector where the matrix is transposed (measures as
rows, vertices as columns).

## Slots

- `geometry`:

  SurfaceGeometry instance representing the surface structure

- `indices`:

  Integer vector of valid surface node indices

- `data`:

  Matrix of values, where columns represent different measures and rows
  correspond to surface nodes

## Note

The number of rows in 'data' must match the number of nodes in
'geometry'.

## See also

[`SurfaceGeometry`](SurfaceGeometry.md),
[`NeuroSurface`](NeuroSurface.md)

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

# Define indices for all vertices
indices <- 1:4

# Create a Matrix with multiple measures for each vertex
# Each row corresponds to a vertex, each column to a different measure
require(Matrix)
vertex_data <- Matrix(
  c(0.5, 1.2, 0.8,    # Measure 1 values for vertices 1-4
    0.7, 0.3, 1.5, 0.9,  # Measure 2 values for vertices 1-4
    1.1, 0.6, 0.4, 1.3), # Measure 3 values for vertices 1-4
  nrow = 4, ncol = 3,
  byrow = FALSE
)
#> Warning: data length [11] is not a sub-multiple or multiple of the number of rows [4]

# Create the NeuroSurfaceVector object
neuro_surface_vector <- new("NeuroSurfaceVector",
                         geometry = geometry,
                         indices = indices,
                         data = vertex_data)

# The data matrix now maps multiple values to each surface vertex
# Vertex 1 has values: 0.5, 0.7, 1.1
# Vertex 2 has values: 1.2, 0.3, 0.6
# etc.
# }
```
