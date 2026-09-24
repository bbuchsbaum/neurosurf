# Deterministic scalar-first surface rasterization

Rasterizes a cortical triangle mesh with a per-sample z-buffer and
barycentric interpolation of the scalar field. Thresholding and palette
mapping occur after scalar interpolation at each sample. Surfaces are
lit with interpolated (Phong) normals, and coarse meshes are subdivided
for display. The backend is a CPU implementation and requires neither
OpenGL nor a browser.

## Usage

``` r
render_surface_rgba(
  geometry,
  vertex_values,
  anatomy_metric = NULL,
  cortex_mask = NULL,
  camera = c("lateral", "medial", "dorsal", "ventral"),
  camera_mode = c("canonical", "presentation"),
  presentation_obliquity = 7,
  width = 1200L,
  height = 750L,
  threshold = 0,
  tail = c("two_sided", "positive", "negative"),
  palette = NULL,
  limits = NULL,
  overlay_alpha = 1,
  alpha_ramp = 0,
  antialias = 3L,
  margin = 0.04,
  medial_wall = c("shade", "mask", "outline"),
  outer_contour = TRUE,
  outer_contour_color = "#6E6E6ED9",
  background = "#FFFFFF",
  lighting = TRUE,
  subdivide = "auto",
  pixel_scale = NULL,
  return_buffers = FALSE,
  anatomy_style = c("publication", "continuous", "binary"),
  anatomy_midpoint = NULL,
  anatomy_invert = "auto",
  anatomy_range = c(0.7, 0.93)
)
```

## Arguments

- geometry:

  A \[SurfaceGeometry\] object.

- vertex_values:

  Numeric value per vertex.

- anatomy_metric:

  Optional numeric anatomy metric per vertex, such as sulcal depth or
  curvature. Values are robustly scaled and mapped to the grey levels in
  \`anatomy_range\`, with sulci darker than gyri.

- cortex_mask:

  Logical cortex-domain mask per vertex. Overlay color is never painted
  on triangles touching a masked vertex.

- camera:

  One of \`"lateral"\`, \`"medial"\`, \`"dorsal"\`, or \`"ventral"\`.

- camera_mode:

  Strict canonical orthographic output or a presentation camera with a
  small, explicit obliquity.

- presentation_obliquity:

  Obliquity in degrees used only for \`camera_mode = "presentation"\`.

- width, height:

  Output dimensions in pixels.

- threshold:

  Non-negative absolute scalar threshold.

- tail:

  Threshold tail: two-sided, positive, or negative.

- palette:

  Character vector of at least two colours, ordered from low to high
  values. With a positive threshold the palette spans only the
  suprathreshold range: for a two-sided map the first half of the
  colours is the negative ramp and the second half the positive ramp.
  \`NULL\` uses a default suited to \`tail\`.

- limits:

  Numeric scalar-color limits.

- overlay_alpha:

  Maximum overlay opacity.

- alpha_ramp:

  Width of the opacity ramp above threshold.

- antialias:

  Integer supersampling factor per axis.

- margin:

  Fractional panel margin.

- medial_wall:

  Whether masked-domain triangles are neutrally shaded, omitted, or
  independently outlined.

- outer_contour:

  Draw an anti-aliased silhouette contour, about one pixel wide, where
  covered cortex touches background connected to the image exterior.
  Enclosed holes and depth discontinuities are not treated as outer
  contour.

- outer_contour_color:

  Contour colour; an alpha channel sets its opacity.

- background:

  Background color.

- lighting:

  \`TRUE\` for the default two-light Blinn-Phong shading, \`FALSE\` for
  flat (unlit) rendering, or a named list overriding any of \`ambient\`,
  \`key\`, \`fill\`, \`specular\`, \`shininess\`, \`key_dir\`,
  \`fill_dir\` (view-space directions: x right, y up, z toward the
  viewer), \`overlay_shading\` (share of the shading applied to overlay
  colour), and \`sky\` (hemispheric ambient fraction). Overlay shading
  is normalized so a surface facing the camera shows exactly the
  colour-bar colour.

- subdivide:

  \`"auto"\` subdivides coarse meshes for display until the median
  projected edge is at most four pixels (up to three levels); \`FALSE\`
  disables it; an integer sets the number of levels.

- pixel_scale:

  Output pixels per surface coordinate unit (usually mm). \`NULL\` fits
  the surface to the panel; a common value gives several panels one
  scale.

- return_buffers:

  Include scalar and depth buffers for diagnostics.

- anatomy_style:

  Underlay mapping: legacy publication shading, centered continuous
  contrast, or binary folding contrast.

- anatomy_midpoint:

  Passed to \[normalize_surface_anatomy()\].

- anatomy_invert:

  Reverse the metric's polarity. \`"auto"\` (default) inverts metrics
  that are larger in sulci (FreeSurfer \`sulc\` and \`curv\`) and keeps
  metrics that are larger on gyral crowns (such as \[curvature()\]),
  judged by the metric's correlation with the displayed mesh's mean
  curvature. The decision is recorded in the provenance.

- anatomy_range:

  Dark and light gray levels in \[0, 1\].

## Value

A \`surface_rgba\` list with raw RGBA, coverage, and overlay-alpha
arrays plus camera and rendering provenance.

## Details

Display subdivision uses the interpolating modified-butterfly scheme:
original vertices keep their positions and values, and each new vertex's
scalar value is clamped to the range of its edge's endpoints, so
subdivision smooths threshold contours without creating suprathreshold
regions that linear interpolation would not.
