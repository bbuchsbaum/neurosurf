# Derive a stable anatomical curvature underlay

Averages computed curvature over mesh neighbors before display. This is
an anatomical texture operation: neither coordinates nor statistical
overlays are modified. Prefer a native curvature/sulcal metric when
available.

## Usage

``` r
anatomical_curvature(geometry, iterations = 5L)
```

## Arguments

- geometry:

  Matching folded (usually white)
  [SurfaceGeometry](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry.md).

- iterations:

  Number of adjacency averaging steps, including the vertex itself. Zero
  returns
  [`curvature()`](https://bbuchsbaum.github.io/neurosurf/reference/curvature-methods.md)
  unchanged. This is not an FWHM in mm.

## Value

Numeric anatomical metric with one value per vertex.
