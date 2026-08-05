# Define a named scalar layer for a surface scene

Define a named scalar layer for a surface scene

## Usage

``` r
surface_layer(
  name,
  values,
  indices = NULL,
  colormap = "viridis",
  limits = NULL,
  opacity = 1,
  units = NULL,
  legend = list(),
  metadata = list(),
  provenance = list(),
  visible = TRUE,
  threshold = NULL
)
```

## Arguments

- name:

  Stable layer name.

- values:

  A numeric vector for a unilateral scene, or a named list with one
  numeric vector per hemisphere.

- indices:

  Optional 1-based vertex indices with the same structure and lengths as
  \`values\`. Omit for full-vertex maps.

- colormap:

  A surfview colormap name or a character vector of colors.

- limits:

  Optional finite display limits. By default they are computed from
  finite values across hemispheres.

- opacity:

  Numeric scalar between zero and one.

- units:

  Optional measurement units.

- legend:

  A list with optional \`title\`, \`units\`, \`visible\`, and
  \`metadata\` fields.

- metadata, provenance:

  Arbitrary lists carried into the manifest.

- visible:

  Whether this layer is a candidate for initial selection.

- threshold:

  Optional static threshold pair. This preserves explicit legacy display
  thresholds; report mode does not add a threshold control.

## Value

A validated layer specification for \[surface_scene()\].
