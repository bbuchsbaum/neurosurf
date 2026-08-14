# Render a neurosurf plot using rgl

Most code calls [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
on a
[`surface_plot`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md)
object, which renders and draws in one step. This function exposes the
rendering stage alone for custom composition.

## Usage

``` r
render_surface_plot(x, offscreen = TRUE, scale = c(2, 2), crop = TRUE)
```

## Arguments

- x:

  A `"neurosurf_plot"` object.

- offscreen:

  Logical; if `TRUE`, rendering is performed with `rgl.useNULL = TRUE`
  so that plots can be captured as images. A real GL context is
  attempted first for better antialiasing.

- scale:

  Numeric vector of length 2 giving a supersampling factor for the
  offscreen snapshot. Values above 1 render at higher resolution before
  downscaling for smoother edges. Defaults to `c(2, 2)`.

- crop:

  Logical; if `TRUE`, automatically crops away white/empty margins from
  each snapshot to avoid the "tiny brain" effect in grids.

## Value

A list containing rendered panel images (with aspect ratios) and layout
information. This is a low-level helper intended to be wrapped by
higher-level figure drawing utilities.

## See also

[`surface_plot`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md),
[`add_surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md),
[`view_surface`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md)

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
p <- surface_plot(geom)
if (interactive()) {
  rendered <- render_surface_plot(p)
}
# }
```
