# Compute Connected Components on a Surface

This method identifies connected components on a NeuroSurface object
based on a given threshold.

## Usage

``` r
# S4 method for class 'NeuroSurfaceVector'
conn_comp(x, threshold, index = 1)

# S4 method for class 'NeuroSurface'
conn_comp(x, threshold)
```

## Arguments

- x:

  A `NeuroSurface` object representing the surface data.

- threshold:

  A numeric vector of length 2 specifying the lower and upper thresholds
  for including vertices in the components.

- index:

  the index/column of the underlying data matrix to cluster

## Value

A list containing two `NeuroSurface` objects:

- index:

  A `NeuroSurface` object where each vertex is labeled with its
  component index.

- size:

  A `NeuroSurface` object where each vertex is labeled with the size of
  its component.

## Details

This method computes connected components on the surface by:

1.  Thresholding the surface data using the provided threshold values.

2.  Creating a subgraph of the surface mesh containing only the vertices
    that pass the threshold.

3.  Identifying connected components in this subgraph.

4.  Assigning component indices and sizes to the original vertices.

Vertices that do not pass the threshold are assigned a value of 0 in
both output surfaces.

## See also

[`NeuroSurface`](NeuroSurface.md),
[`components`](https://r.igraph.org/reference/components.html)

## Examples

``` r
# Load a sample surface from the package
surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
surf_geom <- readAsc(surf_file)
#> Error in readAsc(surf_file): could not find function "readAsc"

# Create random data for the surface with some clusters
n_vertices <- nrow(coords(surf_geom))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'coords': object 'surf_geom' not found
set.seed(123)
random_data <- rnorm(n_vertices, mean = 0, sd = 1)
#> Error: object 'n_vertices' not found

# Create a few clusters of higher values
cluster_centers <- sample(1:n_vertices, 5)
#> Error: object 'n_vertices' not found
g <- graph(surf_geom)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'graph': object 'surf_geom' not found

# For each cluster center, set nearby vertices to higher values
for (center in cluster_centers) {
  # Get neighbors within 2 steps
  neighbors <- unlist(igraph::neighborhood(g, 2, center))
  random_data[neighbors] <- random_data[neighbors] + 2
}
#> Error: object 'cluster_centers' not found

# Create a NeuroSurface object
neuro_surf <- NeuroSurface(geometry = surf_geom,
                          indices = 1:n_vertices,
                          data = random_data)
#> Error: object 'surf_geom' not found

# Find connected components with threshold c(-Inf, 1.5)
# This will identify clusters where values are >= 1.5
components <- conn_comp(neuro_surf, c(-Inf, 1.5))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'conn_comp': object 'neuro_surf' not found

# Check the number of components found
max(series(components$index))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'series': object 'components' not found

# Check the size of the largest component
max(series(components$size))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'series': object 'components' not found

# Count vertices in components of size >= 10
sum(series(components$size) >= 10)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'series': object 'components' not found
```
