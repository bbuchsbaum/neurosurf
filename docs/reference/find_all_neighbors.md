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
surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
surf <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e31f94425b/neurosurf/extdata/std.8_lh.inflated.asc

# Create edge weights (using uniform weights for simplicity)
g <- graph(surf)
edge_weights <- rep(1, length(igraph::E(g)))

# Find neighbors within a 10mm radius for the first 5 vertices
neighbors <- find_all_neighbors(surf, radius = 10,
                               edgeWeights = edge_weights,
                               nodes = 1:5,
                               distance_type = "geodesic")

# Check the number of neighbors found for the first vertex
nrow(neighbors[[1]])
#> [1] 221

# Look at the first few neighbors of the first vertex
head(neighbors[[1]])
#>      i   j d
#> [1,] 1   1 0
#> [2,] 1  55 1
#> [3,] 1  27 1
#> [4,] 1 125 1
#> [5,] 1  13 1
#> [6,] 1  90 1
```
