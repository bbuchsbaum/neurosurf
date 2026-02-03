# Plot method for neurosurf_plot objects

This is a convenience wrapper that renders a multi-panel surface layout
and draws it to a new grid device.

## Usage

``` r
# S3 method for class 'neurosurf_plot'
plot(x, ...)
```

## Arguments

- x:

  A `"neurosurf_plot"` object.

- ...:

  Additional arguments passed to
  [`draw_surface_plot`](draw_surface_plot.md).

## Value

Invisibly returns the input `neurosurf_plot` object.
