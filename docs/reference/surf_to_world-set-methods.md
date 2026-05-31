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

[`surf_to_world`](surf_to_world-methods.md),
[`SurfaceGeometry-class`](SurfaceGeometry-class.md)

## Examples

``` r
# \donttest{
surf_file <- system.file("extdata", "std.8_lh.white.asc", package = "neurosurf")
geom <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpzRF1jx/temp_libpathf6cf495866b/neurosurf/extdata/std.8_lh.white.asc
new_xform <- diag(4)
surf_to_world(geom) <- new_xform
# }
```
