# Show a surface plot in one step

This is a convenience wrapper around
[`surface_plot`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md),
[`add_surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md),
and
[`plot.neurosurf_plot`](https://bbuchsbaum.github.io/neurosurf/reference/plot.neurosurf_plot.md).
It is intended for quick inspection and simple publication-style plots.

## Usage

``` r
show_surface_plot(
  lh,
  rh = NULL,
  data = NULL,
  views = c("lateral", "medial"),
  layout = c("grid", "row", "column"),
  cmap = "viridis",
  irange = NULL,
  color_range = NULL,
  thresh = NULL,
  show_colorbar = TRUE,
  outline = FALSE,
  background = "white",
  zoom = 2,
  margin = 0.03,
  trim = FALSE,
  file = NULL,
  width = 1200,
  height = 900,
  ...
)
```

## Arguments

- lh, rh:

  Either `SurfaceGeometry` objects or file paths that can be read by
  [`read_surf_geometry`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_geometry.md).
  At least one must be provided.

- data:

  Optional numeric vector or list of vectors containing vertex-wise data
  to plot. If a single numeric vector is supplied, it is split across
  hemispheres in left-to-right order based on the vertex counts of the
  surfaces. If `NULL`, a plain surface is shown.

- views:

  Character vector of named views to display for each hemisphere. See
  [`surface_plot`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md)
  for valid values.

- layout:

  One of `"grid"`, `"row"`, or `"column"` controlling how views and
  hemispheres are arranged.

- cmap:

  Colour map for the data layer: either a vector of colours or a single
  palette name understood by
  [`hcl.colors`](https://rdrr.io/r/grDevices/palettes.html) (for example
  `"viridis"`, `"inferno"`, `"magma"`). See
  [`add_surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md).

- irange:

  Optional numeric vector of length 2 giving the minimum and maximum
  values for the colour scale. Alias for `color_range`.

- color_range:

  Optional numeric vector of length 2 giving the minimum and maximum
  values for the colour scale.

- thresh:

  Optional numeric threshold band. A length-2 value is passed to the
  colour mapper as `c(lower, upper)`; a scalar is treated as a symmetric
  band around zero.

- show_colorbar:

  Logical; if `TRUE`, draw a colour bar for the data layer.

- outline:

  Logical; if `TRUE`, the supplied `data` are treated as ROI labels and
  boundaries are drawn instead of a filled map.

- background:

  Background colour for the figure (also used as the PNG canvas colour
  and for background-aware cropping). Defaults to `"white"`; any solid
  colour such as `"#222222"` works.

- zoom:

  Numeric camera zoom passed to
  [`surface_plot`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md).
  Because panels are auto-cropped to their content, `zoom` does not
  change how much of each panel the brain fills; use `margin` to control
  whitespace.

- margin:

  Fraction of background kept around each cropped brain (default
  `0.03`); smaller values pack the brains more tightly.

- trim:

  Logical; if `TRUE` and `file` is supplied, crop the uniform-background
  border from the saved PNG so the brains fill the image (native
  equivalent of ImageMagick `-trim`). The output dimensions become the
  content bounding box, so `width`/`height` act as an upper bound rather
  than a fixed canvas.

- file:

  Optional PNG output path. If supplied, the plot is drawn to this file
  instead of the active graphics device.

- width, height:

  Pixel dimensions used when `file` is supplied.

- ...:

  Additional arguments passed through to
  [`add_surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md)
  (for example `alpha`, `alpha_range`, `alpha_gamma`, `outline_col`,
  `outline_lwd`). Pass `alpha = "soft"` (or a per-vertex `alpha` vector)
  for data-modulated opacity that mirrors
  `neuroim2::plot_overlay(ov_alpha_mode = "soft")`.

## Value

Invisibly returns the underlying `"neurosurf_plot"` object. The plot is
drawn as a side-effect.

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
if (interactive()) {
  show_surface_plot(geom, data = rnorm(nrow(coords(geom))))
}
# }
```
