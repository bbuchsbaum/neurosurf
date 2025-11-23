# VertexColoredNeuroSurface

This function creates a VertexColoredNeuroSurface object, which
represents a surface with explicit colors assigned to each vertex.

## Usage

``` r
VertexColoredNeuroSurface(geometry, indices, colors, data = NULL)
```

## Arguments

- geometry:

  A `SurfaceGeometry` object representing the underlying surface
  structure.

- indices:

  An integer vector specifying the indices of valid surface nodes.

- colors:

  A character vector of hex color codes for each vertex.

- data:

  An optional numeric vector of data values. If not provided, defaults
  to zeros. This parameter exists for compatibility with the parent
  NeuroSurface class but is not used for coloring (colors are specified
  directly via the `colors` parameter).

## Details

This object represents a surface where each vertex has an explicitly
assigned color, bypassing any data-to-color mapping. This is useful when
you want direct control over vertex colors, such as when displaying
parcellation results or pre-computed color schemes.

## See also

[`SurfaceGeometry`](SurfaceGeometry.md),
[`NeuroSurface`](NeuroSurface.md),
[`ColorMappedNeuroSurface`](ColorMappedNeuroSurface.md)

## Examples

``` r
# \donttest{
# Load a sample surface geometry
surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
surf_geom <- read_surf_geometry(surf_file)
#> Error in SurfaceGeometrySource(surface_name): file.exists(surface_name) is not TRUE

# Get first 100 vertices for this example
n_verts <- min(100, nrow(coords(surf_geom)))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'coords': object 'surf_geom' not found
vertex_indices <- 1:n_verts
#> Error: object 'n_verts' not found

# Create colors based on vertex coordinates (x-position)
x_coords <- coords(surf_geom)[vertex_indices, 1]
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'coords': object 'surf_geom' not found
vertex_colors <- ifelse(x_coords > median(x_coords), "#FF6B6B", "#4ECDC4")
#> Error: object 'x_coords' not found

# Create the VertexColoredNeuroSurface object
colored_surf <- VertexColoredNeuroSurface(geometry = surf_geom,
                                          indices = vertex_indices,
                                          colors = vertex_colors)
#> Error: object 'vertex_indices' not found

# Print the object summary
print(colored_surf)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'print': object 'colored_surf' not found

# The object can now be plotted with the specified colors
# plot(colored_surf) # Requires rgl package
# }
```
