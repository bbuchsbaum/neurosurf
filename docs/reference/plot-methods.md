# plot a surface

plot a surface

## Usage

``` r
# S4 method for class 'SurfaceGeometry,missing'
plot(
  x,
  vals = NA,
  cmap = grDevices::gray(seq(0, 1, length.out = 255)),
  vert_clrs = NULL,
  irange = range(vals),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = "lightgray",
  ...
)

# S4 method for class 'NeuroSurface,missing'
plot(
  x,
  cmap = grDevices::gray(seq(0, 1, length.out = 255)),
  vert_clrs = NULL,
  irange = range(x@data, na.rm = TRUE),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = "lightgray",
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
  bgcol = "lightgray",
  ...
)

# S4 method for class 'ColorMappedNeuroSurface,missing'
plot(
  x,
  vert_clrs = NULL,
  alpha = 1,
  specular = "black",
  bgcol = "lightgray",
  ...
)

# S4 method for class 'VertexColoredNeuroSurface,missing'
plot(x, alpha = 1, specular = "black", bgcol = "lightgray", ...)
```

## Arguments

- x:

  the surface to plot

- vals:

  An optional numeric vector containing data values for each vertex on
  the surface. If provided and \`vert_clrs\` is NULL, these values are
  mapped to colors using \`cmap\` and \`irange\`.

- cmap:

  A vector of colors (e.g., hex codes) defining the color map used when
  \`vals\` is provided and \`vert_clrs\` is NULL. Defaults to
  \`rainbow(256)\`.

- vert_clrs:

  An optional character vector of hex color codes for each vertex. If
  provided, these colors directly override any coloring derived from
  \`vals\` and \`cmap\`. The length should match the number of vertices
  in \`surfgeom\`.

- irange:

  An optional numeric vector of length 2, \`c(min, max)\`. Specifies the
  range of \`vals\` to map onto the \`cmap\`. Values outside this range
  will be clamped to the min/max colors. Defaults to the full range of
  \`vals\`.

- thresh:

  An optional numeric vector of length 2, \`c(lower, upper)\`. Vertices
  with \`vals\` \*outside\* this range (i.e., \`\< lower\` or \`\>
  upper\`) are made fully transparent. This is applied \*after\* the
  general \`alpha\`. Defaults to NULL (no thresholding).

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
  base color for the surface. If \`vals\` or \`vert_clrs\` are provided,
  this color is blended with the data/vertex colors. Defaults to
  "lightgray".

- ...:

  extra args to send to `view_surface`
