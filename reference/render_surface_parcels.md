# Render a parcel map on a surface

Rasterizes a labelled surface with deferred per-pixel shading. Each
output pixel is supersampled; every sample resolves its parcel using the
prepared membership or constrained smooth partition. Parcels with a
finite value (or an explicit colour) are filled and outlined; all others
show the shaded anatomical underlay with faint boundaries. The medial
wall is drawn flat and light.

## Usage

``` r
render_surface_parcels(
  x,
  values = NULL,
  colors = NULL,
  camera = c("lateral", "medial", "dorsal", "ventral"),
  camera_mode = c("canonical", "presentation"),
  presentation_obliquity = 7,
  width = 900L,
  height = 620L,
  antialias = 3L,
  palette = c("#3B4CC0", "#F7F7F7", "#B40426"),
  limits = NULL,
  style = surface_parcel_style(),
  margin = 0.01
)
```

## Arguments

- x:

  A \`surface_parcel_prep\` from \[prepare_surface_parcels()\]. Its
  boundary method and parameters are reused without recomputation. In
  smooth mode, samples accepted just outside a face by the rasterizer's
  edge tolerance have negative barycentrics clipped to zero and
  renormalized for label selection only; original weights still
  determine shading/depth.

- values:

  Optional numeric parcel values named by label id. Unnamed values must
  follow the sorted non-zero labels. \`NA\` leaves a parcel unfilled.

- colors:

  Optional fill colours named by label id, used instead of \`values\`
  (e.g. categorical atlas colours).

- camera:

  View: \`"lateral"\`, \`"medial"\`, \`"dorsal"\`, or \`"ventral"\`.

- camera_mode, presentation_obliquity:

  Canonical or slightly oblique camera; see \[render_surface_rgba()\].

- width, height:

  Output raster size in pixels.

- antialias:

  Supersampling factor per axis.

- palette:

  Colours for \`values\`, low to high.

- limits:

  Two increasing numbers mapped to the palette ends. Defaults to the
  symmetric absolute range of \`values\`.

- style:

  A \[surface_parcel_style()\] list.

- margin:

  Fractional margin around the surface.

## Value

A list with \`rgba\` (a \`height\` x \`width\` x 4 raw array with
straight alpha; transparent where the surface is absent), \`labels\`
(the parcel label at each output pixel centre; \`NA\` off-surface),
\`camera\`, and \`provenance\`.
