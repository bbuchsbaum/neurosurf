# Generic Function for Smoothing a Surface or Associated Data

The `smooth` function is a generic function designed to apply smoothing
operations to various surface objects. The specific behavior of the
function depends on the class of the object passed as the `x` argument.
It can be used to smooth the geometry of a surface, the data associated
with a surface, or other related operations depending on the method
implemented for the object's class.

This method applies smoothing to a brain surface geometry object of
class `SurfaceGeometry` using various algorithms. Smoothing is useful
for removing noise and creating a more continuous surface.

## Usage

``` r
smooth(x, ...)

# S4 method for class 'SurfaceGeometry'
smooth(
  x,
  type = c("taubin", "laplace", "HClaplace", "fujiLaplace", "angWeight",
    "surfPreserveLaplace"),
  lambda = 0.7,
  mu = -0.53,
  delta = 0.1,
  iteration = 5
)
```

## Arguments

- x:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the brain surface to be smoothed.

- ...:

  Additional arguments passed to the specific method for smoothing the
  surface object.

- type:

  A character string specifying the smoothing algorithm to use.
  Available options are:

  "taubin"

  :   Applies Taubin smoothing, which preserves the overall shape of the
      surface while reducing noise.

  "laplace"

  :   Performs Laplacian smoothing, which is a basic smoothing method
      that averages the position of each vertex with its neighbors.

  "HClaplace"

  :   Applies a Laplacian smoothing with hard constraints. It preserves
      the boundary vertices and is useful for surfaces with important
      edge features.

  "fujiLaplace"

  :   Uses a Laplacian smoothing method that preserves features more
      aggressively than the basic Laplacian method.

  "angWeight"

  :   Performs angle-weighted smoothing, which considers the angles
      between faces to preserve sharp features.

  "surfPreserveLaplace"

  :   Applies surface-preserving Laplacian smoothing, aiming to maintain
      the original surface's key characteristics while smoothing.

- lambda:

  A numeric value that controls the amount of smoothing. Higher values
  lead to more aggressive smoothing. This parameter is particularly
  relevant for Taubin and Laplacian smoothing methods.

- mu:

  A numeric value used in Taubin smoothing to control shrinkage. A value
  close to zero reduces shrinkage, while a negative value can help in
  shape preservation.

- delta:

  A numeric value used in certain smoothing algorithms to adjust the
  influence of smoothing (e.g., in surface-preserving methods).

- iteration:

  An integer specifying the number of smoothing iterations to apply.
  More iterations result in a smoother surface but can also lead to
  excessive flattening.

## Value

The function returns the smoothed `SurfaceGeometry` object with the
updated mesh.

## Details

The `smooth` function provides a common interface for smoothing
operations on different types of surface objects. The actual smoothing
process varies based on the class of the object provided:

- For [`SurfaceGeometry`](SurfaceGeometry-class.md) objects, the
  function smooths the surface geometry, modifying the shape of the mesh
  to reduce noise.

- For [`NeuroSurface`](NeuroSurface-class.md) objects, the function
  smooths the data values associated with each vertex, preserving the
  surface geometry but producing a smoother dataset.

Users should refer to the specific method documentation for the class of
object they are working with to understand the exact behavior and
parameters.

## See also

`smooth,SurfaceGeometry-method`,
[`smooth,NeuroSurface-method`](smooth-NeuroSurface-method.md)

[`vcgSmooth`](https://rdrr.io/pkg/Rvcg/man/vcgSmooth.html) for more
details on the underlying smoothing algorithms.

## Examples

``` r
# \donttest{
  sg <- example_surface_geometry()
  smoothed_geom <- smooth(sg, type="taubin", lambda=0.7, iteration=10)
# }

# Load a surface file from the extdata directory
surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
surface <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpFhvBlN/temp_libpathff9947c5f204/neurosurf/extdata/std.8_lh.inflated.asc

# Apply Taubin smoothing to the brain surface
smoothed_surface1 <- smooth(surface, type = "taubin", lambda = 0.5, mu = -0.5, iteration = 10)

# Apply surface-preserving Laplacian smoothing
smoothed_surface2 <- smooth(surface, type = "surfPreserveLaplace", iteration = 5)
```
