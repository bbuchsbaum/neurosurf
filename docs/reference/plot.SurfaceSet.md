# Plot method for SurfaceSet objects

Plot method for SurfaceSet objects

## Usage

``` r
# S3 method for class 'SurfaceSet'
plot(x, y, label = NULL, ...)
```

## Arguments

- x:

  A [`SurfaceSet`](SurfaceSet-class.md).

- y:

  Ignored (for S3 compatibility).

- label:

  Optional surface label to display; defaults to the set's default.

- ...:

  Additional arguments passed to [`view_surface`](view_surface.md).

## Value

Invisibly returns the object ID(s) from the RGL scene.

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
ss <- surface_set(inflated = geom)
if (interactive()) {
  plot(ss)
}
# }
```
