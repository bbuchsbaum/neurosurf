# SurfaceGeometry Class

The \`SurfaceGeometry\` class represents a three-dimensional surface
consisting of a set of triangle vertices. It encapsulates the mesh
structure, graph representation, and hemisphere information of a brain
surface.

## Details

This class is fundamental for representing brain surface geometries in
neuroimaging analyses. The mesh slot contains the vertex and face
information, while the graph slot provides a network representation of
the surface topology. The hemi slot specifies which hemisphere the
surface represents.

## Slots

- `mesh`:

  An object of class `mesh3d` representing the underlying 3D mesh
  structure.

- `graph`:

  An object of class `igraph` representing the underlying graph
  structure of the surface.

- `hemi`:

  A character string indicating the hemisphere of the surface ("left",
  "right", or "both").

## See also

[`mesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html),
[`igraph`](https://r.igraph.org/reference/aaa-igraph-package.html)

## Examples

``` r
# \donttest{
# Create a simple tetrahedron mesh
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
surface <- new("SurfaceGeometry",
               mesh = mesh,
               graph = graph,
               hemi = "left")
# }
```
