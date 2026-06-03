# Set Surface-to-World Transform

Sets the 4x4 affine transformation matrix that converts surface
coordinates to world (scanner RAS) coordinates.

## Usage

``` r
surf_to_world(x) <- value

# S4 method for class 'SurfaceGeometry,matrix'
surf_to_world(x) <- value
```

## Arguments

- x:

  An object containing surface geometry (e.g., `SurfaceGeometry`).

- value:

  A 4x4 numeric matrix representing the affine transformation.

## Value

The modified object with the updated transform.

## See also

[`surf_to_world`](https://bbuchsbaum.github.io/neurosurf/reference/surf_to_world-methods.md),
[`SurfaceGeometry-class`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)

## Examples

``` r
# \donttest{
surf_file <- system.file("extdata", "std.8_lh.white.asc", package = "neurosurf")
geom <- read_surf_geometry(surf_file)
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.white.asc
new_xform <- diag(4)
surf_to_world(geom) <- new_xform
# }
```
