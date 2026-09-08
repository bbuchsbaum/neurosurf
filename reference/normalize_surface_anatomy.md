# Normalize an anatomical surface underlay

Converts curvature or sulcal depth to a centered display metric in
\[-0.5, 0.5\], suitable for the surfview curvature layer. This changes
only anatomical shading, never statistical values or surface
coordinates.

## Usage

``` r
normalize_surface_anatomy(
  metric,
  style = c("continuous", "binary"),
  midpoint = NULL,
  invert = FALSE
)
```

## Arguments

- metric:

  Finite numeric anatomy values, one per vertex.

- style:

  Continuous robust scaling or binary folding contrast.

- midpoint:

  Boundary between dark and light. NULL uses the median, including for
  the historical \[0, 1\] output of \[curvature()\]. Use zero for a
  native signed metric whose anatomical boundary is zero.

- invert:

  Reverse dark/light polarity. Select this from the source metric's sign
  convention; signs differ between curvature producers.

## Value

A numeric vector in \[-0.5, 0.5\]. Constant metrics remain neutral.
