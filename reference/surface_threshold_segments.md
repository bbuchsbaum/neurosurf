# Marching-triangle threshold segments

Computes exact linear threshold crossings from the same vertex-scalar
model used by \[render_surface_rgba()\]. Positive and negative levels
are handled as independent regions for two-sided maps.

## Usage

``` r
surface_threshold_segments(
  vertices,
  faces,
  values,
  threshold,
  tail = c("two_sided", "positive", "negative")
)
```

## Arguments

- vertices:

  N by 2 or N by 3 projected vertex coordinates.

- faces:

  F by 3 one-based vertex indices.

- values:

  Numeric scalar per vertex.

- threshold:

  Positive threshold magnitude.

- tail:

  Two-sided, positive, or negative levels.

## Value

A data frame with two rows per threshold segment.
