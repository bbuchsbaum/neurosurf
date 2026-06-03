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
  color_range = NULL,
  show_colorbar = TRUE,
  outline = FALSE,
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

  Character string naming a colour map for the data layer.

- color_range:

  Optional numeric vector of length 2 giving the minimum and maximum
  values for the colour scale.

- show_colorbar:

  Logical; if `TRUE`, draw a colour bar for the data layer.

- outline:

  Logical; if `TRUE`, the supplied `data` are treated as ROI labels and
  boundaries are drawn instead of a filled map.

- ...:

  Additional arguments passed through to
  [`add_surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/add_surface_layer.md)
  (for example `alpha`, `outline_col`, `outline_lwd`).

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
