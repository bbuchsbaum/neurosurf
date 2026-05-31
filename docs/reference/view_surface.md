# Display a 3D Brain Surface using RGL

Renders a 3D brain surface mesh using the \`rgl\` package. This function
provides flexible options for coloring the surface based on data values
or predefined colors, adjusting transparency, controlling lighting,
setting viewpoints, and overlaying spherical markers.

## Usage

``` r
view_surface(
  surfgeom,
  vals = NA,
  cmap = grDevices::rainbow(256, alpha = 1),
  vert_clrs = NULL,
  bgcol = "lightgray",
  alpha = 1,
  add_normals = TRUE,
  thresh = NULL,
  irange = NULL,
  specular = "black",
  lit = NULL,
  viewpoint = c("lateral", "medial", "ventral", "dorsal", "anterior", "posterior"),
  new_window = TRUE,
  offset = c(0, 0, 0),
  zoom = 1,
  spheres = NULL,
  spheres_map_surface = NULL,
  spheres_map_label = NULL,
  spheres_as_vertices = FALSE,
  vectors = NULL,
  vector_vertices = NULL,
  vector_scale = NULL,
  vector_color = "red",
  vector_alpha = 0.8,
  vector_lwd = 1.5,
  vals_vertices = NULL,
  vals_smoothing = c("auto", "nearest"),
  vals_smoothing_steps = 20,
  label = NULL,
  ...
)
```

## Arguments

- surfgeom:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the 3D brain surface mesh to be displayed, or a
  [`SurfaceSet`](SurfaceSet-class.md) containing multiple variants.

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

- bgcol:

  A single hex color code or a vector of hex color codes used as the
  base color for the surface. If \`vals\` or \`vert_clrs\` are provided,
  this color is blended with the data/vertex colors. Defaults to
  "lightgray".

- alpha:

  A numeric value between 0 (fully transparent) and 1 (fully opaque)
  controlling the overall transparency of the surface. Defaults to 1.

- add_normals:

  Logical. If TRUE (default), surface normals are calculated and added
  to the mesh, which improves the appearance of lighting effects.

- thresh:

  An optional numeric vector of length 2, \`c(lower, upper)\`. Vertices
  with \`vals\` \*outside\* this range (i.e., \`\< lower\` or \`\>
  upper\`) are made fully transparent. This is applied \*after\* the
  general \`alpha\`. Defaults to NULL (no thresholding).

- irange:

  An optional numeric vector of length 2, \`c(min, max)\`. Specifies the
  range of \`vals\` to map onto the \`cmap\`. Values outside this range
  will be clamped to the min/max colors. Defaults to the full range of
  \`vals\`.

- specular:

  The color of specular highlights on the surface, affecting its
  shininess. Can be a color name (e.g., "white") or hex code. Defaults
  to "black" for a matte look. Set to a brighter colour for a glossier
  appearance.

- lit:

  Logical. If `TRUE`, enables lighting effects on the surface. If
  `FALSE`, disables lighting for a flat appearance. If `NULL` (default),
  automatically sets to `TRUE` for interactive sessions and `FALSE` when
  knitting (when `rgl.useNULL` is `TRUE`).

- viewpoint:

  A character string specifying a predefined view (e.g., "lateral",
  "medial", "ventral", "dorsal", "anterior", "posterior"). The actual
  view depends on the hemisphere (\`surfgeom@hemi\`, e.g.,
  "left_lateral"). Alternatively, a 4x4 transformation matrix defining a
  custom view. Defaults to "lateral".

- new_window:

  Logical. If TRUE (default), opens a new \`rgl\` window for the plot.
  If FALSE, attempts to plot in the currently active \`rgl\` window
  (useful for updates or within Shiny apps).

- offset:

  A numeric vector of length 3 specifying a translation offset \`c(x, y,
  z)\` applied to the surface coordinates before rendering. Defaults to
  \`c(0, 0, 0)\`.

- zoom:

  A numeric value controlling the camera zoom level. Defaults to 1 (no
  zoom). Values \> 1 zoom in, \< 1 zoom out.

- spheres:

  An optional data frame to draw spheres at specific locations on or
  near the surface. Must contain columns \`x\`, \`y\`, \`z\`
  (coordinates), and \`radius\`. Can optionally include a \`color\`
  column (hex codes or color names) for individual sphere colors
  (defaults to black). Alternatively, supply a \`vertex\` column
  (1-based vertex ids) and set `spheres_as_vertices = TRUE` to position
  foci by vertex.

- spheres_map_surface:

  Optional `SurfaceGeometry`, `SurfaceSet`, or file path used to map
  sphere coordinates to the nearest vertex on that surface before
  snapping to `surfgeom`. Assumes both surfaces share the same vertex
  ordering (e.g., white -\> inflated).

- spheres_map_label:

  Optional surface label to use when `spheres_map_surface` is a
  `SurfaceSet`.

- spheres_as_vertices:

  Logical; if `TRUE`, interpret the \`vertex\` column of `spheres` as
  1-based vertex ids on `surfgeom` rather than raw coordinates.

- vectors:

  Optional matrix (n x 3) of XYZ vectors to draw as line glyphs.

- vector_vertices:

  Optional vertex ids matching rows of `vectors` when they are defined
  on a subset of vertices.

- vector_scale:

  Optional numeric scale factor for vectors. If `NULL`, a heuristic
  scale based on mesh extent and vector magnitudes is used.

- vector_color:

  Colour for the vectors (single value or vector).

- vector_alpha:

  Opacity for the vectors (0–1).

- vector_lwd:

  Numeric line width for vector glyphs.

- vals_vertices:

  Optional integer vector of 1-based vertex ids corresponding to
  \`vals\` when \`length(vals) \< n_vertices\`. Enables sparse data
  inputs.

- vals_smoothing:

  One of \`"auto"\` (default) or \`"nearest"\`. When using sparse data,
  \`"auto"\` diffuses values with neighbor averaging after nearest fill;
  \`"nearest"\` performs nearest-neighbour fill only.

- vals_smoothing_steps:

  Integer number of smoothing iterations applied when \`vals_smoothing =
  "auto"\`. Ignored otherwise.

- label:

  Optional surface label to select when \`surfgeom\` is a `SurfaceSet`.
  Defaults to the set's \`default_label\`.

- ...:

  Additional arguments passed directly to \`rgl::shade3d\` for
  fine-grained control over rendering (e.g., \`lit\`, \`smooth\`).

## Value

Invisibly returns the object ID(s) of the shape(s) added to the RGL
scene by \`rgl::shade3d\`. This can be useful for modifying the scene
later.

## Details

\*\*Coloring:\*\* Surface vertex colors are determined by the following
priority: 1. \`vert_clrs\`: If provided, these specific hex colors are
used. 2. \`vals\` & \`cmap\`: If \`vals\` is provided and \`vert_clrs\`
is NULL, \`vals\` are mapped to \`cmap\` based on \`irange\`. 3.
\`bgcol\`: If neither \`vert_clrs\` nor \`vals\` are used for coloring,
\`bgcol\` is applied uniformly. If \`bgcol\` is specified alongside
\`vert_clrs\` or \`vals\`, the colors are blended based on the \`alpha\`
parameter.

\*\*Transparency:\*\* Overall transparency is set by \`alpha\`.
Additional threshold-based transparency can be applied using \`thresh\`
when \`vals\` are provided. Vertices with values outside the \`thresh\`
range become fully transparent.

\*\*Lighting:\*\* \`add_normals=TRUE\` is recommended for realistic
lighting. The \`specular\` parameter controls the shininess.

\*\*Viewpoint:\*\* Predefined viewpoints (\`"lateral"\`, \`"medial"\`,
etc.) are automatically adjusted based on the hemisphere specified in
\`surfgeom@hemi\` (e.g., "lh" results in "left_lateral"). If \`hemi\` is
unknown, the current \`rgl\` view is used unless a custom 4x4 matrix is
provided.

\*\*Performance:\*\* Rendering very large surfaces or surfaces with
complex coloring/transparency can be computationally intensive.

## See also

[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html),
[`spheres3d`](https://dmurdoch.github.io/rgl/dev/reference/spheres.html),
[`view3d`](https://dmurdoch.github.io/rgl/dev/reference/viewpoint.html),
[`SurfaceGeometry`](SurfaceGeometry.md)

## Examples

``` r
# \donttest{
surf_geom <- example_surface_geometry()
if (interactive()) {
  view_surface(surf_geom, viewpoint = "lateral")
}
# }
```
