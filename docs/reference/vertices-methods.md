# Extract Vertices from a Surface Object

Extracts the vertices from a surface object, providing a standardized
interface across different surface representations.

## Usage

``` r
vertices(x, ...)

# S4 method for class 'SurfaceGeometry'
vertices(x, indices)

# S4 method for class 'NeuroSurface'
vertices(x)

# S4 method for class 'NeuroSurfaceVector'
vertices(x, indices)
```

## Arguments

- x:

  An object representing a surface.

- ...:

  Additional arguments passed to methods.

- indices:

  a vector of indices specifying the valid surface nodes.

## Value

A matrix or data structure containing vertex information.

## See also

[`nodes`](nodes-methods.md), [`faces`](faces-methods.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vertex_data <- vertices(surface_obj)
num_vertices <- nrow(vertex_data)
} # }
```
