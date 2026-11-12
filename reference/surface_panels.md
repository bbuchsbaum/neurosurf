# Arrange rendered panels as small multiples

Lays out
[`render_surface_rgba`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md)
panels in one row, sets a label under each surface, and adds the shared
colour key of the first panel's scale (see
[`surface_colorbar_grob`](https://bbuchsbaum.github.io/neurosurf/reference/surface_colorbar_grob.md))
and optional reference swatches. Use it for comparisons that
[`surface_figure`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
does not cover: different values, steps of a pipeline, or rendering
options.

## Usage

``` r
surface_panels(
  panels,
  labels = names(panels),
  legend = TRUE,
  legend_title = NULL,
  references = NULL,
  reference_opacity = 0.4,
  label_family = ""
)

# S3 method for class 'surface_panels'
plot(x, ...)

# S3 method for class 'surface_panels'
print(x, ...)
```

## Arguments

- panels:

  A list of `surface_rgba` panels, drawn left to right.

- labels:

  Text under each panel; defaults to `names(panels)`.

- legend:

  Draw the colour key of the first panel's scale.

- legend_title:

  Text above the key.

- references:

  Optional named character vector of reference colours, drawn as
  labelled swatches beside the key, e.g.
  `c("Intraparietal ROI" = "#238b45")`.

- reference_opacity:

  Fill opacity of the swatches, matching the opacity the references were
  rendered with.

- label_family:

  Font family for labels, e.g. `"mono"` when the labels are code.

- x:

  A `surface_panels` object.

- ...:

  Unused.

## Value

A `surface_panels` object;
[`print()`](https://rdrr.io/r/base/print.html) or
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws it on the
current device.

## Examples

``` r
geom <- example_surface_geometry()
v <- coords(geom)[, 1]
panels <- list(
  dorsal = render_surface_rgba(geom, v, camera = "dorsal",
                               width = 120, height = 120, antialias = 1),
  lateral = render_surface_rgba(geom, v, camera = "lateral",
                                width = 120, height = 120, antialias = 1)
)
surface_panels(panels, legend_title = "x")
```
