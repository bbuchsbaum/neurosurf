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
# Load a surface file from the extdata directory
surf_file <- system.file("extdata", "sample_surface.asc", package = "neurosurf")
surface <- readAsc(surf_file)
#> Error in readAsc(surf_file): could not find function "readAsc"

# Create some random data for the surface vertices
n_vertices <- nrow(coords(surface))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'coords': object 'surface' not found
random_data <- rnorm(n_vertices)
#> Error: object 'n_vertices' not found

# Create a NeuroSurface object with the surface and data
neuro_surf <- NeuroSurface(geometry = surface,
                          indices = 1:n_vertices,
                          data = random_data)
#> Error: object 'surface' not found

# Apply smoothing with different radii
smoothed_small <- smooth(neuro_surf, sigma = 2)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'smooth': object 'neuro_surf' not found
smoothed_large <- smooth(neuro_surf, sigma = 6)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'smooth': object 'neuro_surf' not found

# The original geometry is preserved, but the data is smoothed
# Compare a small section of data before and after smoothing
head(random_data)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': object 'random_data' not found
head(series(smoothed_large))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': error in evaluating the argument 'x' in selecting a method for function 'series': object 'smoothed_large' not found
```
