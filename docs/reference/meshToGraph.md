# Construct a Graph from Mesh Vertices and Faces

This function creates an igraph object representing the connectivity
structure of a 3D mesh based on its vertices and triangular faces.

## Usage

``` r
meshToGraph(vertices, nodes)
```

## Arguments

- vertices:

  A numeric matrix with 3 columns representing the x, y, and z
  coordinates of vertices. Each row corresponds to a vertex.

- nodes:

  A numeric matrix where each row represents a triangular face,
  containing 0-based indices of three vertices that form the face.

## Value

An `igraph` object representing the mesh connectivity. The graph has the
following attributes:

- Vertex attributes: "x", "y", and "z" coordinates from the vertices
  matrix

- Edge attribute: "dist" (Euclidean distance between connected vertices)

## Details

The function converts a triangular mesh into a graph representation
where:

- Vertices of the graph correspond to vertices of the mesh

- Edges of the graph correspond to the edges of triangular faces in the
  mesh

The function performs the following steps:

1.  Extracts all unique edges from the triangular faces

2.  Creates an undirected graph from these edges

3.  Simplifies the graph to remove duplicate edges and loops

4.  Calculates Euclidean distances between connected vertices

5.  Adds vertex coordinates and edge distances as attributes to the
    graph

Note that the input `nodes` matrix should use 0-based indexing (starting
from 0), as the function will increment indices by 1 when creating the
graph.

## See also

[`SurfaceGeometry`](SurfaceGeometry.md),
[`graph_from_edgelist`](https://r.igraph.org/reference/graph_from_edgelist.html)

## Examples

``` r
# \donttest{
# Create a simple cube mesh with 8 vertices
vertices <- matrix(c(
  0, 0, 0,  # vertex 1
  1, 0, 0,  # vertex 2
  1, 1, 0,  # vertex 3
  0, 1, 0,  # vertex 4
  0, 0, 1,  # vertex 5
  1, 0, 1,  # vertex 6
  1, 1, 1,  # vertex 7
  0, 1, 1   # vertex 8
), ncol = 3, byrow = TRUE)

# Define triangular faces with 0-based indices
faces <- matrix(c(
  # bottom face (z=0)
  0, 1, 2,
  0, 2, 3,
  # top face (z=1)
  4, 5, 6,
  4, 6, 7,
  # front face (y=0)
  0, 1, 5,
  0, 5, 4,
  # back face (y=1)
  2, 3, 7,
  2, 7, 6,
  # left face (x=0)
  0, 3, 7,
  0, 7, 4,
  # right face (x=1)
  1, 2, 6,
  1, 6, 5
), ncol = 3, byrow = TRUE)

# Create the graph representation of the mesh
graph <- meshToGraph(vertices, faces)

# Examine the graph properties
cat("Number of vertices:", igraph::vcount(graph), "\n")
#> Number of vertices: 8 
cat("Number of edges:", igraph::ecount(graph), "\n")
#> Number of edges: 18 

# Plot the graph if igraph is available
if (interactive() && requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("rgl", quietly = TRUE)) {
  # First visualize the mesh
  rgl::open3d()
  mesh <- rgl::tmesh3d(
    vertices = t(vertices),
    indices = t(faces) + 1,  # rgl uses 1-based indexing
    homogeneous = FALSE
  )
  rgl::shade3d(mesh, col = "lightblue")
  rgl::title3d(main = "Original Mesh")

  # Visualize the graph connections using the 3D coordinates
  rgl::open3d()
  # Plot vertices
  rgl::points3d(vertices[,1], vertices[,2], vertices[,3], size = 10, col = "red")
  # Plot edges
  edges <- igraph::as_edgelist(graph)
  for (i in 1:nrow(edges)) {
    v1 <- edges[i, 1]
    v2 <- edges[i, 2]
    coords <- vertices[c(v1, v2), ]
    rgl::lines3d(coords[,1], coords[,2], coords[,3], col = "black", lwd = 2)
  }
  rgl::title3d(main = "Graph Representation")
}
# }
```
