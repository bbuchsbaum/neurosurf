# Create a surface plot specification

Create a surface plot specification

## Usage

``` r
surface_plot(
  lh,
  rh = NULL,
  views = c("lateral", "medial"),
  layout = c("grid", "row", "column"),
  mirror_views = FALSE,
  flip = FALSE,
  zoom = 2,
  background = "white",
  brightness = 0.5
)
```

## Arguments

- lh, rh:

  Either `SurfaceGeometry` objects or file paths that can be read by
  [`read_surf_geometry`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_geometry.md).
  At least one must be provided.

- views:

  Character vector of named views to display for each hemisphere. Valid
  values include `"lateral"`, `"medial"`, `"ventral"`, `"dorsal"`,
  `"anterior"`, and `"posterior"`. Defaults to `c("lateral","medial")`.

- layout:

  One of `"grid"`, `"row"`, or `"column"` controlling how views and
  hemispheres are arranged.

- mirror_views:

  Logical; if `TRUE`, reverse the order of the right hemisphere views
  for `"row"` and `"column"` layouts so that they mirror the left
  hemisphere.

- flip:

  Logical; if `TRUE` and both hemispheres are present, flip the
  left/right ordering in the layout (useful for anterior views).

- zoom:

  Numeric zoom factor passed through to the underlying
  [`view_surface`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md)
  calls.

- background:

  Background colour for the rgl scene.

- brightness:

  Baseline brightness for a plain surface when no layers are added.
  Value in \\\[0,1\]\\.

## Value

An object of class `"neurosurf_plot"` that can be further modified with
[`add_surface_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md)
and rendered with
[`render_surface_plot()`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_plot.md)
or
[`draw_surface_plot()`](https://bbuchsbaum.github.io/neurosurf/reference/draw_surface_plot.md).

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
p <- surface_plot(geom)
p <- add_surface_layer(p, data = rnorm(nrow(coords(geom))))
# }
```
