# Colour key for surface panels

Builds the colour bar that
[`surface_figure`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
draws, as a grid grob, for figures composed by hand from
[`render_surface_rgba`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md)
panels. Suprathreshold tails are drawn as separate ramps with the
threshold gap between them, and an opacity ramp above the threshold is
shown in the key as it appears on the cortex.

## Usage

``` r
surface_colorbar_grob(
  x = NULL,
  limits = NULL,
  threshold = 0,
  tail = c("two_sided", "positive", "negative"),
  palette = NULL,
  overlay_alpha = 1,
  alpha_ramp = 0,
  title = NULL
)
```

## Arguments

- x:

  A `surface_rgba` panel whose colour scale the key should show, or
  `NULL` to give the scale explicitly.

- limits, threshold, tail, palette, overlay_alpha, alpha_ramp:

  The colour scale, as for
  [`render_surface_rgba`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md).
  Taken from `x` when it is supplied.

- title:

  Text set beside the bar, typically the statistic.

## Value

A grid grob. Draw it with
[`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html) in a
viewport about 12 mm tall.

## Examples

``` r
key <- surface_colorbar_grob(limits = c(-4, 4), threshold = 1.5,
                             title = "z")
grid::grid.newpage()
grid::grid.draw(key)
```
