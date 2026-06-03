# Find the nearest surface vertex to a 3D point

Find the nearest surface vertex to a 3D point

## Usage

``` r
find_nearest_vertex(surface, point)
```

## Arguments

- surface:

  A `SurfaceGeometry`.

- point:

  Numeric vector of length 3.

## Value

Integer vertex index (1-based) of the closest vertex.

## Examples

``` r
geom <- example_surface_geometry()
find_nearest_vertex(geom, c(0.9, 0, 0))
#> [1] 2
```
