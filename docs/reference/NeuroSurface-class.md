# NeuroSurface

a three-dimensional surface consisting of a set of triangle vertices
with one value per vertex.

## Details

The NeuroSurface class is a fundamental representation of surface-based
neuroimaging data. It combines geometric information about a brain
surface with data values mapped to each vertex of that surface.

The class consists of three core components:

- `geometry`: The underlying 3D surface structure, containing vertex
  coordinates, face definitions, and topological information

- `indices`: Identifiers for the subset of vertices in the geometry that
  have associated data values

- `data`: A numeric vector containing one value per vertex, representing
  measurements such as cortical thickness, functional activation, or any
  other surface-mapped metric

This class serves as the foundation for more specialized surface
representations like `ColorMappedNeuroSurface`,
`VertexColoredNeuroSurface`, and `LabeledNeuroSurface`. It facilitates
common operations such as visualization, statistical analysis, and
spatial processing of surface-based neuroimaging data.

## Slots

- `geometry`:

  the surface geometry, an instance of `SurfaceGeometry`

- `indices`:

  an `integer` vector specifying the subset of valid surface nodes
  encoded in the `geometry` object.

- `data`:

  the 1-D vector of data value at each vertex of the mesh

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

# Create data values for each vertex
vertex_data <- c(0.5, 1.2, 0.8, 1.5)  # example values for the vertices

# Create the NeuroSurface object
neuro_surface <- new("NeuroSurface",
                    geometry = geometry,
                    indices = indices,
                    data = vertex_data)

# The data values are now mapped to the surface vertices
# and can be visualized or analyzed
# }
```
