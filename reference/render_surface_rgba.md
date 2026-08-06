# Deterministic scalar-first surface rasterization

Rasterizes a cortical triangle mesh with a per-pixel z-buffer and
barycentric interpolation of the scalar field. Thresholding and palette
mapping occur after scalar interpolation at each sample. The backend is
a CPU implementation and requires neither OpenGL nor a browser.

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
  palette = c("#3B4CC0", "#F7F7F7", "#B40426"),
  limits = NULL,
  overlay_alpha = 0.85,
  alpha_ramp = 0,
  antialias = 2L,
  margin = 0.04,
  medial_wall = c("shade", "mask", "outline"),
  outer_contour = TRUE,
  outer_contour_color = "#595959",
  background = "#FBFBF8",
  return_buffers = FALSE
)
```

## Arguments

- geometry:

  A \[SurfaceGeometry\] object.

- vertex_values:

  Numeric value per vertex.

- anatomy_metric:

  Optional numeric anatomy metric per vertex. Values are robustly scaled
  to \[0, 1\] and modulate a quiet grey substrate.

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

  Character vector of at least two colors.

- limits:

  Numeric scalar-color limits.

- overlay_alpha:

  Maximum overlay opacity.

- alpha_ramp:

  Width of the opacity ramp above threshold.

- antialias:

  Integer supersampling factor.

- margin:

  Fractional panel margin.

- medial_wall:

  Whether masked-domain triangles are neutrally shaded, omitted, or
  independently outlined.

- outer_contour:

  Draw a one-pixel contour only where covered cortex touches background
  connected to the image exterior. Enclosed holes and depth
  discontinuities are not treated as outer contour.

- outer_contour_color:

  Contour color.

- background:

  Background color.

- return_buffers:

  Include scalar and depth buffers for diagnostics.

## Value

A \`surface_rgba\` list with raw RGBA, coverage, and overlay-alpha
arrays plus camera and rendering provenance.
