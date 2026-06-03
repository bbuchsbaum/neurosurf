# Shiny bindings for surfwidget

Output and render functions for using surfwidget within Shiny
applications and interactive Rmd documents.

Update the configuration of an existing surfwidget.

Modify the minimum and maximum values used for data mapping.

Set the threshold limits for showing surface data.

Replace the per-vertex colors of an existing surfwidget.

Adjust the overall opacity of an existing surfwidget.

Adjust the zoom level of a surfwidget widget.

Change the automatic rotation speed of a surfwidget widget.

## Usage

``` r
surfwidgetOutput(outputId, width = "100%", height = "400px")

renderSurfwidget(expr, env = parent.frame(), quoted = FALSE)

updateSurfwidgetConfig(session, id, config)

updateIRange(widget, min, max)

updateThreshold(widget, min, max)

updateVertexColors(widget, colors)

updateAlpha(widget, alpha)

updateZoom(widget, zoom)

updateRotationSpeed(widget, speed)
```

## Arguments

- outputId:

  output variable to read from

- width, height:

  Must be a valid CSS unit (like `'100%'`, `'400px'`, `'auto'`) or a
  number, which will be coerced to a string and have `'px'` appended.

- expr:

  An expression that generates a surfwidget

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.

- session:

  The `session` object passed to function given to `shinyServer`.

- id:

  The ID of the surfwidget output.

- config:

  A list of configuration options to update. See
  [`surfwidget`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
  for details on available options.

- widget:

  A surfwidget htmlwidget object.

- min:

  Numeric lower bound of the threshold.

- max:

  Numeric upper bound of the threshold.

- colors:

  A vector of colors to apply to each vertex.

- alpha:

  Numeric opacity value between 0 (transparent) and 1 (opaque).

- zoom:

  Numeric zoom factor.

- speed:

  Numeric rotation speed.

## Value

For `surfwidgetOutput`, a Shiny UI element for displaying the widget.
For `renderSurfwidget`, a server-side render function. For
`updateSurfwidgetConfig`, `updateIRange`, `updateThreshold`,
`updateVertexColors`, `updateAlpha`, `updateZoom`, and
`updateRotationSpeed`, the modified widget object (invisibly).

## Details

Sends a custom message of type `"neurosurf-surfwidget-config"` to update
the widget configuration on the client.
