# Find Node Neighbors in a Surface Mesh

Identifies all neighboring nodes within a specified radius for a given
surface mesh.

## Usage

``` r
find_all_neighbors(
  surf,
  radius,
  edgeWeights,
  nodes = NULL,
  distance_type = c("euclidean", "geodesic", "spherical")
)
```

## Arguments

- surf:

  A SurfaceGeometry object or igraph object representing the mesh

- radius:

  Numeric; the spatial radius within which to search for neighbors. Must
  be positive.

- edgeWeights:

  Numeric vector; weights for edges used in distance computation. Length
  must equal the number of edges.

- nodes:

  Integer vector; subset of nodes to find neighbors for. If NULL, uses
  all nodes

- distance_type:

  Character; type of distance metric to use: "euclidean", "geodesic", or
  "spherical"

## Value

A list of matrices, each containing neighbor information:

- i:

  Source node index

- j:

  Neighbor node index

- d:

  Distance between nodes

## Details

The function supports three distance metrics: Euclidean, geodesic, and
spherical. For spherical distances, the surface is assumed to be a
sphere. The internal k-nearest-neighbor search is capped at
`vcount(g) - 1` to avoid requesting more neighbors than exist in the
graph.

## Examples

``` r
# Load a sample inflated surface from the package
surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
surf <- readAsc(surf_file)
#> Error in readAsc(surf_file): could not find function "readAsc"

# Create edge weights (using uniform weights for simplicity)
g <- graph(surf)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'graph': object 'surf' not found
edge_weights <- rep(1, length(E(g)))
#> Error in E(g): could not find function "E"

# Find neighbors within a 10mm radius for the first 5 vertices
neighbors <- find_all_neighbors(surf, radius = 10,
                               edgeWeights = edge_weights,
                               nodes = 1:5,
                               distance_type = "geodesic")
#> Error: object 'surf' not found

# Check the number of neighbors found for the first vertex
nrow(neighbors[[1]])
#> Error: object 'neighbors' not found

# Look at the first few neighbors of the first vertex
head(neighbors[[1]])
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': object 'neighbors' not found
```
