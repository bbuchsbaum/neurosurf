# VertexColoredNeuroSurface

A three-dimensional surface consisting of a set of triangle vertices
with one color per vertex.

## Value

An object of class `VertexColoredNeuroSurface`.

## Details

This class extends `NeuroSurface` by adding per-vertex coloring
functionality. The `colors` slot contains a vector of hex color codes
that define the color for each vertex. Unlike `ColorMappedNeuroSurface`,
this class does not use a colormap or data mapping, but instead directly
specifies colors for each vertex.

## Slots

- `geometry`:

  The surface geometry, an instance of `SurfaceGeometry` or `SurfaceSet`

- `indices`:

  An `integer` vector specifying the subset of valid surface nodes
  encoded in the `geometry` object

- `colors`:

  A character vector of hex color codes representing the color of each
  vertex

## See also

[`view_surface`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md)

## Examples

``` r
# \donttest{
# First create a simple tetrahedron mesh
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

# Create placeholder data (required by NeuroSurface parent class)
vertex_data <- c(0, 0, 0, 0)  # Not used for coloring

# Define explicit colors for each vertex
vertex_colors <- c(
  "#FF0000",  # Red for vertex 1
  "#00FF00",  # Green for vertex 2
  "#0000FF",  # Blue for vertex 3
  "#FFFF00"   # Yellow for vertex 4
)

# Create the VertexColoredNeuroSurface object
colored_vertices <- new("VertexColoredNeuroSurface",
                       geometry = geometry,
                       indices = indices,
                       data = vertex_data,
                       colors = vertex_colors)

# In this example, each vertex has an explicit color:
# - Vertex 1 is red
# - Vertex 2 is green
# - Vertex 3 is blue
# - Vertex 4 is yellow
# }
```
