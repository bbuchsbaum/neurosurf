# Smooth Data on a NeuroSurface Object

This method applies smoothing to the data values associated with a
[`NeuroSurface`](NeuroSurface-class.md) object. Unlike the geometric
smoothing applied to [`SurfaceGeometry`](SurfaceGeometry-class.md), this
function smooths the scalar values (e.g., intensity or activation)
associated with each vertex on the surface.

## Usage

``` r
# S4 method for class 'NeuroSurface'
smooth(x, sigma = 5, ...)
```

## Arguments

- x:

  A [`NeuroSurface`](NeuroSurface-class.md) object containing the brain
  surface and associated data to be smoothed.

- sigma:

  A numeric value specifying the smoothing radius. This defines the
  neighborhood around each vertex used to compute the smoothed value.
  Default is 5.

- ...:

  Additional arguments passed to the smoothing function.

## Value

A new `NeuroSurface` object with the smoothed data values. The geometry
remains unchanged.

## Details

The smoothing process involves averaging the data values within a
geodesic neighbourhood of each vertex. For every vertex the function
uses [`find_all_neighbors`](find_all_neighbors.md) to locate all
vertices within the radius specified by `sigma`. The smoothed value is
the mean of the vertex's own value and those of its neighbours.
Increasing `sigma` results in broader smoothing because more neighbours
are included in the average.

The smoothing is particularly useful when working with noisy data or
when a smoother representation of the underlying signal is desired. It
is commonly applied in neuroimaging to enhance visualization or prepare
data for further analysis.

## See also

[`smooth,SurfaceGeometry-method`](smooth-methods.md) for smoothing the
geometry of a surface.

## Examples

``` r
# Create a tiny example surface and data
surface <- example_surface_geometry()
n_vertices <- nrow(coords(surface))
random_data <- rnorm(n_vertices)

neuro_surf <- NeuroSurface(geometry = surface,
                          indices = seq_len(n_vertices),
                          data = random_data)

# Apply smoothing with different radii
smoothed_small <- smooth(neuro_surf, sigma = 1)
smoothed_large <- smooth(neuro_surf, sigma = 2)

# The original geometry is preserved, but the data is smoothed
head(series(smoothed_large, indices(smoothed_large)))
#>            [,1]       [,2]       [,3]       [,4]
#> [1,] -0.2277551 -0.2277551 -0.2277551 -0.2277551
```
