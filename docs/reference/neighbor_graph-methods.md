# Construct Neighborhood Graph from Surface Mesh

This generic function constructs a neighborhood graph from a surface
mesh using edge weights. It allows for flexible definition of
neighborhoods based on edge radius and custom edge weights.

## Usage

``` r
neighbor_graph(x, radius, edgeWeights = missing(), nodes = missing(), ...)

# S4 method for class 'igraph,numeric,missing,missing'
neighbor_graph(
  x,
  radius,
  edgeWeights = NULL,
  nodes = NULL,
  distance_type = c("geodesic", "euclidean", "spherical")
)

# S4 method for class 'SurfaceGeometry,numeric,missing,missing'
neighbor_graph(
  x,
  radius,
  edgeWeights = NULL,
  nodes = NULL,
  distance_type = c("geodesic", "euclidean", "spherical")
)

# S4 method for class 'SurfaceGeometry,numeric,numeric,missing'
neighbor_graph(
  x,
  radius,
  edgeWeights,
  distance_type = c("geodesic", "euclidean", "spherical")
)

# S4 method for class 'SurfaceGeometry,numeric,numeric,integer'
neighbor_graph(
  x,
  radius,
  edgeWeights,
  nodes,
  distance_type = c("geodesic", "euclidean", "spherical")
)

# S4 method for class 'SurfaceGeometry,numeric,missing,integer'
neighbor_graph(
  x,
  radius,
  nodes,
  distance_type = c("geodesic", "euclidean", "spherical")
)
```

## Arguments

- x:

  An object representing a surface mesh. The specific class depends on
  the method implementation.

- radius:

  Numeric. The edge radius defining the neighborhood extent.

- edgeWeights:

  Numeric vector. Custom weights for edges, used to define edge
  distances.

- nodes:

  Integer vector. The subset of nodes to use in graph construction. If
  NULL, all nodes are used.

- ...:

  Additional arguments passed to methods.

- distance_type:

  the type of distance metric to use

## Value

An object representing the constructed neighborhood graph. The specific
class depends on the method implementation.

## Details

The neighborhood graph is constructed by considering edges within the
specified radius and applying the provided edge weights. This function
is particularly useful in neuroimaging analyses for defining local
connectivity on brain surfaces.

## See also

[`graph`](graph-methods.md), [`vertices`](vertices-methods.md)

## Examples

``` r
# \donttest{
# Assuming 'surface_mesh' is a pre-defined surface mesh object
graph <- neighbor_graph(surface_mesh, radius = 5,
                        edgeWeights = runif(nrow(surface_mesh$vertices)),
                        nodes = 1:1000)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'neighbor_graph': object 'surface_mesh' not found
# }

# \donttest{
g <- make_graph("Zachary")
#> Error in make_graph("Zachary"): could not find function "make_graph"
E(g)$dist <- runif(ecount(g))
#> Error in ecount(g): could not find function "ecount"
neighbor_g <- neighbor_graph(g, radius = 0.5, distance_type = "geodesic")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'neighbor_graph': object 'g' not found
# }
```
