# ColorMappedNeuroSurface

A three-dimensional surface consisting of a set of triangle vertices
with one value per vertex, mapped to colors using a specified colormap
and range.

## Value

An object of class `ColorMappedNeuroSurface`

## Details

This class extends `NeuroSurface` by adding color mapping functionality.
The `cmap` slot contains a vector of hex color codes that define the
colormap. The `irange` slot specifies the range of data values to be
mapped to the colormap. The `thresh` slot defines the visibility
thresholds: data values below `thresh[1]` or above `thresh[2]` are
visible, while values in between are not visible.

The color mapping process works as follows:

1.  Data values are linearly mapped to the range \[0,1\] based on
    `irange`

2.  These normalized values are used to interpolate colors from the
    `cmap`

3.  Values falling between the thresholds in `thresh` are marked as
    invisible

This approach is particularly useful for visualizing statistical maps
(e.g., t-statistics) where researchers are often interested in
highlighting values above or below certain significance thresholds.

## Slots

- `geometry`:

  The surface geometry, an instance of `SurfaceGeometry` or `SurfaceSet`

- `indices`:

  An `integer` vector specifying the subset of valid surface nodes
  encoded in the `geometry` object

- `data`:

  The 1-D vector of data values at each vertex of the mesh

- `cmap`:

  A character vector of hex color codes representing the colormap

- `irange`:

  A numeric vector of length 2 specifying the low and high values for
  color mapping

- `thresh`:

  A numeric vector of length 2 specifying the low and high thresholds
  for visibility

## See also

[`view_surface`](view_surface.md), `plot-methods`

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

# Create data values for each vertex
vertex_data <- c(-1.5, -0.5, 0.8, 2.0)  # example values for the vertices

# Define a simple colormap (blue to red)
colormap <- c("#0000FF", "#FFFFFF", "#FF0000")

# Define intensity range for mapping data to colors
intensity_range <- c(-2.0, 2.0)

# Define thresholds (values between -0.5 and 0.5 will be invisible)
thresholds <- c(-0.5, 0.5)

# Create the ColorMappedNeuroSurface object
colored_surface <- new("ColorMappedNeuroSurface",
                      geometry = geometry,
                      indices = indices,
                      data = vertex_data,
                      cmap = colormap,
                      irange = intensity_range,
                      thresh = thresholds)

# In this example:
# - Vertex 1 (-1.5) will be visible and colored blue (below lower threshold)
# - Vertex 2 (-0.5) will be exactly at the lower threshold
# - Vertex 3 (0.8) will be visible and colored light red (above upper threshold)
# - Vertex 4 (2.0) will be visible and colored bright red (above upper threshold)
# }
```
