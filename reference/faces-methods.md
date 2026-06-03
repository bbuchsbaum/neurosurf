# Extract Faces from a Surface Object

Extracts the triangle faces from a surface object, providing a
standardized interface across different surface representations.

## Usage

``` r
faces(x, ...)

# S4 method for class 'SurfaceGeometry'
faces(x)

# S4 method for class 'NeuroSurface'
faces(x)

# S4 method for class 'NeuroSurfaceVector'
faces(x)
```

## Arguments

- x:

  An object representing a surface.

- ...:

  Additional arguments passed to methods.

## Value

A matrix where each row represents a triangular face, containing the
1-based vertex indices that form the triangle.

## See also

[`vertices`](https://bbuchsbaum.github.io/neurosurf/reference/vertices-methods.md),
[`nodes`](https://bbuchsbaum.github.io/neurosurf/reference/nodes-methods.md)

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
face_data <- faces(geom)
num_faces <- nrow(face_data)
# }
```
