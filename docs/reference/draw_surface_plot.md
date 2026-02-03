# Draw a static multi-panel surface figure

This is a convenience wrapper around
[`render_surface_plot`](render_surface_plot.md) that arranges rendered
panels into a single static figure using the grid graphics system. It
supersamples, crops whitespace, and preserves per-panel aspect ratios to
avoid tiny or distorted brains when assembled.

## Usage

``` r
draw_surface_plot(
  x,
  colorbar = TRUE,
  cbar_location = c("bottom", "right"),
  cbar_kws = list()
)
```

## Arguments

- x:

  A `"neurosurf_plot"` object.

- colorbar:

  Logical; if `TRUE`, draws one or more colour bars for non-outline
  layers that have `show_colorbar = TRUE`.

- cbar_location:

  Location of colour bars relative to the panel layout. Currently
  supports `"bottom"` (default) or `"right"`.

- cbar_kws:

  Optional list of graphical parameters for colour bars (e.g.,
  `bar_height`, `title_cex`, `label_cex`, `n_ticks`).

## Value

A `grob` object that can be drawn with
[`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html).

## Examples

``` r
# \donttest{
# Requires FreeSurfer-like surface files
# sp <- surface_plot(left = "lh.pial", right = "rh.pial")
# g <- draw_surface_plot(sp)
# grid::grid.draw(g)
# }
```
