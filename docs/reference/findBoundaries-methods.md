# Find Boundaries Between Regions on a Surface

This generic function identifies boundaries between different regions or
parcellations on a surface. The implementation depends on the class of
the input object.

## Usage

``` r
findBoundaries(x, method = "midpoint", ...)

# S4 method for class 'NeuroSurface'
findBoundaries(x, method = c("edge_vertices", "faces"), ...)
```

## Arguments

- x:

  A [`NeuroSurface`](NeuroSurface-class.md) object whose `data` slot
  contains integer ROI labels for each vertex.

- method:

  Boundary method passed to
  [`find_roi_boundaries`](find_roi_boundaries.md). Currently one of
  `"faces"` or `"edge_vertices"`.

- ...:

  Additional arguments passed to
  [`find_roi_boundaries`](find_roi_boundaries.md).

## Value

An object containing boundary information. The specific structure
depends on the method implementation.

A list as returned by [`find_roi_boundaries`](find_roi_boundaries.md).

## Details

This function provides a high-level interface for finding boundaries
between different regions on a surface mesh. It typically returns
coordinates and metadata describing the boundaries between regions.

## See also

[`find_roi_boundaries`](find_roi_boundaries.md)

[`find_roi_boundaries`](find_roi_boundaries.md)
