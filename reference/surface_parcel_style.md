# Style settings for parcel surface rendering

Collects the shading, line, and medial-wall settings used by
\[render_surface_parcels()\]. The defaults target publication figures: a
quiet sulcal-depth underlay, soft directional light, filled parcels
outlined in a darker shade of their own colour, faint boundaries between
unfilled parcels, and a flat light medial wall that reads as "no data".

## Usage

``` r
surface_parcel_style(
  gyrus = 0.9,
  sulcus = 0.77,
  ambient = 0.62,
  diffuse = 0.38,
  light = c(-0.3, 0.45, 0.85),
  medial = 0.875,
  medial_light = 0.75,
  fill_light = 0.4,
  fill_anatomy = 0.08,
  fill_line = list(col = "darken", alpha = 0.35, radius = 1L),
  parcel_line = list(col = "#FFFFFF", alpha = 0.4, radius = 0L),
  medial_line = list(col = "#B5B5B5", alpha = 0.8, radius = 0L),
  contour = list(col = "#8A8A8A", alpha = 0.75, radius = 1L),
  rim_fade = 3L,
  ...
)
```

## Arguments

- gyrus, sulcus:

  Underlay grey levels in \\0, 1\\ for gyral crowns and sulcal fundi.

- ambient, diffuse:

  Lambert lighting weights.

- light:

  Light direction in screen space (x right, y up, z toward the viewer).

- medial:

  Medial-wall grey level.

- medial_light:

  Fraction of the medial-wall tone held constant; the remainder follows
  the lighting, so 1 is completely flat.

- fill_light:

  How strongly lighting modulates parcel fills (0 = flat).

- fill_anatomy:

  How strongly sulcal depth shows through parcel fills.

- fill_line:

  Outline around filled parcels.

- parcel_line:

  Boundaries between unfilled parcels.

- medial_line:

  Boundary between cortex and the medial wall.

- contour:

  Outer silhouette.

- rim_fade:

  Supersampled-pixel distance from the silhouette within which
  unfilled-parcel boundaries are suppressed.

- ...:

  Named overrides of the arguments above, convenient when passing a
  list.

## Value

A \`surface_parcel_style\` list.

## Details

Line specifications are lists with \`col\` (a colour, or \`"darken"\`
for the filled-parcel outline), \`alpha\` in \\0, 1\\, and \`radius\`
(dilation in supersampled pixels; 0 is a hairline). Set \`alpha = 0\` to
disable a line class.
