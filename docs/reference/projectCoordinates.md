# Project 3D Coordinates onto a Surface and Smooth the Values

This function projects a set of 3D coordinates onto a given surface and
creates a [`NeuroSurface`](NeuroSurface-class.md) object with the
smoothed values. The projection is performed by finding the closest
points on the surface, and then a kernel density smoother is applied
locally to produce the final values.

## Usage

``` r
projectCoordinates(surfgeom, points, sigma = 5, ...)
```

## Arguments

- surfgeom:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the surface onto which the coordinates will be projected.

- points:

  A numeric matrix with three columns (x, y, z) representing the 3D
  coordinates to be projected onto the surface.

- sigma:

  A numeric value specifying the smoothing radius for the kernel density
  smoother. Default is 5.

- ...:

  Additional arguments passed to the smoothing function.

## Value

A [`NeuroSurface`](NeuroSurface-class.md) object with the smoothed
values mapped onto the surface.

## Details

The function first projects each 3D coordinate onto the closest point on
the surface defined by `surfgeom`. The values at these projected points
are then smoothed using a kernel density smoother, where the `sigma`
parameter controls the extent of the smoothing. The result is a
`NeuroSurface` object containing the smoothed values, suitable for
further analysis or visualization.

## Examples

``` r
# Load a sample surface from the package
surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
surfgeom <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e357118cac/neurosurf/extdata/std.8_lh.inflated.asc

# Get the surface coordinates
surf_coords <- coords(surfgeom)

# Create some sample 3D coordinates to project
# We'll use a subset of the surface vertices with small random offsets
set.seed(123)
sample_indices <- sample(1:nrow(surf_coords), 50)
sample_coords <- surf_coords[sample_indices, ] + matrix(rnorm(150, 0, 0.5), ncol = 3)

# Project these coordinates onto the surface
projected_surface <- projectCoordinates(surfgeom, sample_coords, sigma = 3)

# Check the result
vals <- series(projected_surface, indices(projected_surface))
max(vals)        # Maximum density value
#> [1] 1
sum(vals > 0)    # Number of vertices with non-zero values
#> [1] 50
```
