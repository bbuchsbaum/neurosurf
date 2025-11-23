# Add a data layer to a surface plot

Add a data layer to a surface plot

## Usage

``` r
add_surface_layer(
  x,
  data,
  cmap = "viridis",
  alpha = 1,
  color_range = NULL,
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
  [`surface_plot`](surface_plot.md).

- data:

  Numeric vector or list specifying vertex-wise data. If a vector, it
  should have length equal to the total number of vertices across
  hemispheres and is split left-to-right. If a list, it may contain
  elements named `"left"` and/or `"right"`.

- cmap:

  Character string naming a colour map. The value is passed through to
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html) to
  generate colours.

- alpha:

  Numeric in \\\[0,1\]\\ controlling layer opacity.

- color_range:

  Optional numeric vector of length 2 giving the minimum and maximum
  data values to map to the colour scale. If `NULL`, the range of `data`
  (ignoring `NA`) is used.

- as_outline:

  Logical; if `TRUE`, the data are treated as labels and are intended to
  be visualised as region outlines rather than a filled map. When
  `as_outline = TRUE`, the layer does not contribute to the filled
  vertex colours; instead its ROI boundaries are rendered as line
  overlays using [`findBoundaries`](findBoundaries-methods.md).

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
