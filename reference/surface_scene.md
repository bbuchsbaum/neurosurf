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

  Optional left and right `SurfaceGeometry` or `SurfaceSet` objects.
  Supply at least one.

- layers:

  A
  [`surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md)
  object or a list of them.

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

  Visual appearance preset. `"paper-light"` is intended for
  light-background figures and does not change widget behavior.

- mode:

  `"report"` for curated controls and fallback behavior, or `"viewer"`
  for a bare interactive viewer.

- asset_mode:

  Default serialization mode: inline base64 or adjacent
  content-addressed files.

## Value

A validated `SurfaceScene` object.

## Details

A scene owns the portable report description, not the statistical
analysis. `preset` changes appearance; `mode` changes viewer behavior.
Inline and adjacent asset modes encode the same typed-array bytes and
preserve missing values. Use
[`surfwidget`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
inside an R Markdown or Quarto document and
[`write_surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
for an ordinary HTML page.

## See also

[`surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md),
[`surfwidget`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md),
[`surface_scene_manifest`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene_manifest.md),
[`write_surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)

## Examples

``` r
geometry <- example_surface_geometry()
scene <- surface_scene(
  left = geometry,
  layers = surface_layer(
    "effect", seq_len(nrow(coords(geometry))), limits = c(1, 4)
  ),
  fallback = "Static left-hemisphere surface figure.",
  alt_text = "Left cortical surface colored by an example effect."
)
scene
#> SurfaceScene 'surface-scene'
#>   hemispheres: left
#>   layers: effect
#>   selected: effect
```
