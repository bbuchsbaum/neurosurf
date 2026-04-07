# Get Surface-to-World Transform

Retrieves the 4x4 affine transformation matrix that converts surface
coordinates to world (scanner RAS) coordinates.

## Usage

``` r
surf_to_world(x)

# S4 method for class 'SurfaceGeometry'
surf_to_world(x)
```

## Arguments

- x:

  An object containing surface geometry (e.g., `SurfaceGeometry`).

## Value

A 4x4 numeric matrix representing the affine transformation.

## Details

The surface-to-world transform handles coordinate system conventions
(e.g., RAS vs LPI), reference space alignment (e.g., MNI305 vs MNI152),
and any embedded transforms from file formats (e.g., GIFTI
CoordinateSystemTransformMatrix).

To transform surface vertices to world coordinates:
`world_coords = surf_to_world(geom) %*% rbind(t(vertices), 1)`

## See also

`surf_to_world<-`, [`SurfaceGeometry-class`](SurfaceGeometry-class.md)

## Examples

``` r
# \donttest{
surf_file <- system.file("extdata", "std.8_lh.white.asc", package = "neurosurf")
geom <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpFhvBlN/temp_libpathff9947c5f204/neurosurf/extdata/std.8_lh.white.asc
xform <- surf_to_world(geom)
print(xform)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
# }
```
