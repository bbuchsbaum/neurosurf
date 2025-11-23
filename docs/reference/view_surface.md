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
  irange = range(vals, na.rm = TRUE),
  specular = "black",
  lit = NULL,
  viewpoint = c("lateral", "medial", "ventral", "dorsal", "anterior", "posterior"),
  new_window = TRUE,
  offset = c(0, 0, 0),
  zoom = 1,
  spheres = NULL,
  ...
)
```

## Arguments

- surfgeom:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the 3D brain surface mesh to be displayed.

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
  (defaults to black).

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
# Assume 'surf_geom' is a SurfaceGeometry object loaded previously
# e.g., surf_geom <- read_surf_geometry("path/to/surface.gii")

# Simple display with default background color
view_surface(surf_geom, viewpoint = "lateral")
#> Error: object 'surf_geom' not found

# Display with curvature coloring (assuming you have curvature data)
# curv_vals <- curvature(surf_geom) # Calculate curvature if needed
# view_surface(surf_geom, vals = curv_vals, cmap = gray.colors(256), viewpoint = "medial")

# Display with specific vertex colors (e.g., based on labels)
# num_verts <- length(nodes(surf_geom))
# colors_for_verts <- sample(c("red", "blue", "green"), num_verts, replace = TRUE)
# view_surface(surf_geom, vert_clrs = colors_for_verts, viewpoint = "ventral")

# Display with thresholding and custom color map
# random_data <- rnorm(length(nodes(surf_geom)))
# view_surface(surf_geom, vals = random_data,
#              cmap = colorRampPalette(c("blue", "white", "red"))(256),
#              thresh = c(-1.5, 1.5), # Make values between -1.5 and 1.5 transparent
#              irange = c(-3, 3),     # Map values from -3 to 3 onto the cmap
#              viewpoint = "posterior")

# Display with spheres marking specific coordinates
sphere_coords <- data.frame(
  x = c(10, -15, 5),
  y = c(20, 0, -10),
  z = c(-5, 25, 15),
  radius = c(3, 4, 2.5),
  color = c("yellow", "cyan", "magenta")
)
view_surface(surf_geom, viewpoint = "lateral", spheres = sphere_coords)
#> Error: object 'surf_geom' not found

# Plot in the current rgl window without opening a new one
# rgl::open3d() # Open window first
# view_surface(surf_geom, new_window = FALSE)
# }
```
