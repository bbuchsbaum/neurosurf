# Add a data layer to a surface plot

Add a data layer to a surface plot

## Usage

``` r
add_surface_layer(
  x,
  data,
  cmap = "viridis",
  alpha = 1,
  alpha_range = NULL,
  alpha_gamma = NULL,
  irange = NULL,
  color_range = NULL,
  thresh = NULL,
  vertices = NULL,
  smoothing = c("auto", "nearest"),
  smoothing_steps = 20,
  as_outline = FALSE,
  zero_transparent = TRUE,
  show_colorbar = TRUE,
  label = NULL,
  outline_col = "black",
  outline_lwd = 1.5,
  outline_offset = 0,
  outline_halo = FALSE,
  outline_halo_col = NULL,
  outline_halo_lwd = NULL,
  outline_rois = NULL,
  outline_lty = c("solid", "dashed"),
  hemi = c("both", "left", "right")
)
```

## Arguments

- x:

  A `"neurosurf_plot"` object created by
  [`surface_plot`](https://bbuchsbaum.github.io/neurosurf/reference/surface_plot.md).

- data:

  Numeric vector or list specifying vertex-wise data. If a vector, it
  should have length equal to the total number of vertices across
  hemispheres and is split left-to-right. If a list, it may contain
  elements named `"left"` and/or `"right"`.

- cmap:

  Colour specification for the layer. May be a vector of two or more
  colours (passed to
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html)), or
  a single character string naming a palette. Palette names understood
  by [`hcl.colors`](https://rdrr.io/r/grDevices/palettes.html) (for
  example `"viridis"`, `"inferno"`, `"magma"`, `"plasma"`, `"cividis"`)
  are resolved via
  [`hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html); see
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
  for the full list. An unrecognised name falls back to a blue-white-red
  ramp with a warning.

- alpha:

  Layer opacity. One of: a single numeric in \\\[0,1\]\\ (uniform
  opacity, the default); a numeric vector of per-vertex opacities
  matching `data` in length and layout (values are clamped to
  \\\[0,1\]\\ and, for sparse `data`, filled/smoothed the same way); or
  the string `"soft"` to derive per-vertex opacity from the data
  magnitude (see `alpha_range`/`alpha_gamma`). Per-vertex and `"soft"`
  alpha let opacity be modulated by the data value, mirroring
  `neuroim2::plot_overlay(ov_alpha_mode = "soft")` for volumes.

- alpha_range:

  Numeric length-2 vector `c(lo, hi)` used when `alpha = "soft"`:
  opacity rises from 0 at `lo` to 1 at `hi` as
  `clamp((|data| - lo)/(hi - lo), 0, 1)`. If `NULL`, defaults to
  `c(0, max(abs(color_range)))`. Ignored unless `alpha = "soft"`.

- alpha_gamma:

  Optional positive exponent applied to the soft-alpha ramp
  (`opacity^alpha_gamma`). Values `> 1` keep low signal fainter; values
  `< 1` lift it. `NULL` (default) is treated as `1` (linear). Ignored
  unless `alpha = "soft"`.

- irange:

  Optional numeric vector of length 2 giving the minimum and maximum
  data values to map to the colour scale. Alias for `color_range`.

- color_range:

  Optional numeric vector of length 2 giving the minimum and maximum
  data values to map to the colour scale. If `NULL`, the range of `data`
  (ignoring `NA`) is used.

- thresh:

  Optional numeric threshold band passed to the colour mapper. A scalar
  is expanded to `c(-abs(thresh), abs(thresh))`.

- vertices:

  Optional vector or list of vertex ids corresponding to the supplied
  `data` when it is defined on a subset of vertices. Use a list with
  elements `left`/`right` for hemisphere-specific subsets.

- smoothing:

  One of `"auto"` (default) or `"nearest"` when using sparse data.
  `"auto"` fills missing vertices by nearest neighbour then applies
  smoothing iterations; `"nearest"` performs only nearest-neighbour
  fill.

- smoothing_steps:

  Integer number of smoothing iterations applied when
  `smoothing = "auto"`. Ignored otherwise.

- as_outline:

  Logical; if `TRUE`, the data are treated as labels and are intended to
  be visualised as region outlines rather than a filled map. When
  `as_outline = TRUE`, the layer does not contribute to the filled
  vertex colours; instead its ROI boundaries are rendered as line
  overlays using
  [`findBoundaries`](https://bbuchsbaum.github.io/neurosurf/reference/findBoundaries-methods.md).

- zero_transparent:

  Logical; if `TRUE`, zeros are turned into `NA` so they render as
  transparent.

- show_colorbar:

  Logical; if `TRUE`, this layer will contribute a colour bar when using
  a figure-level drawing helper.

- label:

  Optional character label identifying the layer (for legends and colour
  bars).

- outline_col:

  Colour to use for ROI boundaries when `as_outline = TRUE`. May be a
  single colour name/hex code or the special value `"auto"`, in which
  case boundaries are coloured by ROI using the layer's `cmap`. Ignored
  for non-outline layers.

- outline_lwd:

  Numeric line width to use when drawing ROI boundaries for outline
  layers. Ignored for non-outline layers.

- outline_offset:

  Numeric scalar giving a small depth offset applied to boundary
  coordinates along the surface normals. This helps avoid z-fighting
  with the underlying mesh. A value around `0.5`–`1` is often sufficient
  for standardised cortical meshes.

- outline_halo:

  Logical; if `TRUE`, draws a two-pass boundary with a thicker halo
  under a thinner main line to improve legibility.

- outline_halo_col:

  Colour used for the halo when `outline_halo = TRUE`. Defaults to a
  light colour if `NULL`.

- outline_halo_lwd:

  Numeric line width for the halo. If `NULL`, a slightly larger width
  than `outline_lwd` is used.

- outline_rois:

  Optional numeric vector of ROI ids to plot boundaries for when
  `as_outline = TRUE`. If `NULL`, boundaries are drawn for all ROIs
  present in the data.

- outline_lty:

  Line type for boundaries, one of `"solid"` or `"dashed"`. Dashed lines
  are approximated by drawing alternating short segments along the
  boundary polyline.

- hemi:

  One of `"both"`, `"left"`, or `"right"`, indicating which hemispheres
  the supplied `data` apply to when a single numeric vector is given.

## Value

A modified `"neurosurf_plot"` object.

## Examples

``` r
# \donttest{
# Requires FreeSurfer-like surface files
# sp <- surface_plot(left = "lh.pial", right = "rh.pial")
# sp <- add_surface_layer(sp, data = rnorm(163842))
# }
```
