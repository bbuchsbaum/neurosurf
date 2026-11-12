# Compute Surface Curvature Vector

Compute Surface Curvature Vector

## Usage

``` r
curvature(x, ...)

curvature(x, ...)

# S4 method for class 'SurfaceGeometry'
curvature(x)
```

## Arguments

- x:

  Object to compute curvature from

- ...:

  Additional arguments

## Value

A numeric vector with one value per vertex: mean curvature rescaled to
\[0, 1\] over the mesh. Higher values are convex (gyral crowns), lower
values concave (sulcal fundi). The rescaling makes it a display index
rather than a quantity in 1/mm; for FreeSurfer-style shading pass it to
[`curv_cols_smooth`](https://bbuchsbaum.github.io/neurosurf/reference/curv_cols_smooth.md).
