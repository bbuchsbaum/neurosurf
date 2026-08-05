# Construct a validated portable surface scene

Construct a validated portable surface scene

## Usage

``` r
surface_scene(
  left = NULL,
  right = NULL,
  layers,
  curvature = NULL,
  selected_layer = NULL,
  id = "surface-scene",
  metadata = list(),
  provenance = list(),
  fallback,
  alt_text,
  preset = "paper-light",
  mode = c("report", "viewer"),
  asset_mode = c("inline", "directory")
)
```

## Arguments

- left, right:

  Optional left and right \`SurfaceGeometry\` or \`SurfaceSet\` objects.
  Supply at least one.

- layers:

  A \`surface_layer()\` object or a list of them.

- curvature:

  Optional numeric vector for a unilateral scene or named hemisphere
  list.

- selected_layer:

  Initially selected layer name. Defaults to the first visible layer,
  then the first layer.

- id:

  Stable scene identifier.

- metadata, provenance:

  Arbitrary lists carried into the manifest.

- fallback:

  Required plain-text fallback content.

- alt_text:

  Required alternative text for the interactive figure.

- preset:

  Visual appearance preset. \`"paper-light"\` is intended for
  light-background figures and does not change widget behavior.

- mode:

  \`"report"\` for curated controls and fallback behavior, or
  \`"viewer"\` for a bare interactive viewer.

- asset_mode:

  Default serialization mode: inline base64 or adjacent
  content-addressed files.

## Value

A validated \`SurfaceScene\` object.
