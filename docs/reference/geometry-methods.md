# Extract Geometry from Surface Object

Extracts the geometric representation from a surface object.

## Usage

``` r
geometry(x)

# S4 method for class 'NeuroSurface'
geometry(x)

# S4 method for class 'NeuroSurfaceVector'
geometry(x)
```

## Arguments

- x:

  A surface object

## Value

A geometry object representing the surface structure

## See also

[`vertices`](vertices-methods.md), [`nodes`](nodes-methods.md)

## Examples

``` r
# \donttest{
# Load a sample surface
surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
surf <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e31f94425b/neurosurf/extdata/std.8_lh.smoothwm.asc
# Create a NeuroSurface with some data
ns <- NeuroSurface(surf, indices = 1:100, data = rnorm(100))
# Extract the geometry
geom <- geometry(ns)
class(geom)
#> [1] "SurfaceGeometry"
#> attr(,"package")
#> [1] "neurosurf"
# }
```
