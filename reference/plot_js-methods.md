# Plot Surface as an HTMLWidget

`plot_js()` is the older name for the interactive HTML widget; new code
should call
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
directly.

## Usage

``` r
plot_js(x, width = NULL, height = NULL, ...)

# S4 method for class 'SurfaceGeometry'
plot_js(x, width = NULL, height = NULL, ...)
```

## Arguments

- x:

  Surface object to plot

- width:

  The width of the widget (optional)

- height:

  The height of the widget (optional)

- ...:

  Additional arguments passed to the plotting function

## Value

An HTMLWidget object

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
widget <- plot_js(geom)
# }
```
