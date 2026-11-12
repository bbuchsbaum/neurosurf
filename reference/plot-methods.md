# Plot a Surface

Renders a surface object using the interactive viewer.

## Usage

``` r
# S4 method for class 'SurfaceGeometry,missing'
plot(
  x,
  vals = NA,
  cmap = NULL,
  vert_clrs = NULL,
  irange = range(vals),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = NULL,
  ...
)

# S4 method for class 'NeuroSurface,missing'
plot(
  x,
  cmap = NULL,
  vert_clrs = NULL,
  irange = range(x@data, na.rm = TRUE),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = NULL,
  ...
)

# S4 method for class 'LabeledNeuroSurface,missing'
plot(
  x,
  cmap = x@cols,
  vert_clrs = NULL,
  irange = range(x@data, na.rm = TRUE),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = NULL,
  ...
)

# S4 method for class 'ColorMappedNeuroSurface,missing'
plot(x, vert_clrs = NULL, alpha = 1, specular = "black", bgcol = NULL, ...)

# S4 method for class 'VertexColoredNeuroSurface,missing'
plot(x, alpha = 1, specular = "black", bgcol = NULL, ...)
```

## Arguments

- x:

  the surface to plot

- vals:

  An optional numeric vector containing data values for each vertex on
  the surface. If provided and `vert_clrs` is NULL, these values are
  mapped to colors using `cmap` and `irange`.

- cmap:

  A vector of colors (e.g., hex codes) defining the color map used when
  `vals` is provided and `vert_clrs` is NULL. The default, `NULL`,
  chooses
  [`surface_palette`](https://bbuchsbaum.github.io/neurosurf/reference/surface_palette.md):
  diverging when `irange` spans zero, sequential otherwise.

- vert_clrs:

  An optional character vector of hex color codes for each vertex. If
  provided, these colors directly override any coloring derived from
  `vals` and `cmap`. The length should match the number of vertices in
  `surfgeom`.

- irange:

  An optional numeric vector of length 2, `c(min, max)`. Specifies the
  range of `vals` to map onto the `cmap`. Values outside this range will
  be clamped to the min/max colors. Defaults to the full range of
  `vals`.

- thresh:

  An optional numeric vector of length 2, `c(lower, upper)`. Values
  *inside* this band are not coloured, so the base colour shows through.
  With a base colour, opacity ramps up over a short band past each limit
  (a quarter of the larger limit's magnitude) so cluster edges fade
  smoothly. Defaults to NULL (no thresholding).

- alpha:

  A numeric value between 0 (fully transparent) and 1 (fully opaque)
  controlling the overall transparency of the surface. Defaults to 1.

- specular:

  The color of specular highlights on the surface, affecting its
  shininess. Can be a color name (e.g., "white") or hex code. Defaults
  to "black" for a matte look. Set to a brighter colour for a glossier
  appearance.

- bgcol:

  A single hex color code or a vector of hex color codes used as the
  base color for the surface. If `vals` or `vert_clrs` are provided,
  this color is blended with the data/vertex colors. The default,
  `NULL`, shades the surface by its curvature (sulci dark, gyri light)
  with
  [`curv_cols_smooth`](https://bbuchsbaum.github.io/neurosurf/reference/curv_cols_smooth.md);
  use `NA` for no base colour.

- ...:

  extra args to send to `view_surface`

## Value

An htmlwidget object for interactive visualization
