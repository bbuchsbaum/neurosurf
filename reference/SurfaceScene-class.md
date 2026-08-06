# A portable cortical surface scene

`SurfaceScene` stores one or two hemisphere geometries and named scalar
layers together with the presentation metadata needed to reconstruct a
report viewer. Use
[`surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md)
to create validated instances.

## Usage

``` r
# S4 method for class 'SurfaceScene'
show(object)
```

## Arguments

- object:

  A `SurfaceScene` to summarize.

## Value

A `SurfaceScene` object.

## Slots

- `id`:

  A stable scene identifier.

- `geometries`:

  A named list of `SurfaceGeometry` objects.

- `curvature`:

  Optional named curvature vectors.

- `layers`:

  A named list of validated surface-layer specifications.

- `selected_layer`:

  The initially selected layer name.

- `metadata`:

  Arbitrary scene metadata.

- `provenance`:

  Arbitrary scene provenance.

- `fallback`:

  Plain-text content shown when JavaScript or WebGL is absent.

- `alt_text`:

  Alternative text for the interactive figure.

- `preset`:

  A visual preset such as `"paper-light"`.

- `mode`:

  Widget behavior mode. `"report"` enables curated controls.

- `asset_mode`:

  Default asset serialization mode.
