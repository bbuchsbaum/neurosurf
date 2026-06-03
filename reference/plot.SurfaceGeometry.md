# Plot method for SurfaceGeometry objects

Plot method for SurfaceGeometry objects

## Usage

``` r
# S3 method for class 'SurfaceGeometry'
plot(x, y, ...)
```

## Arguments

- x:

  A
  [`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
  object.

- y:

  Ignored (for S3 method compatibility).

- ...:

  Additional arguments passed to
  [`view_surface`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md).

## Value

Invisibly returns the object ID(s) from the RGL scene.

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
if (interactive()) {
  plot(geom)
}
# }
```
