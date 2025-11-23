# Create a Surface Widget

This generic function creates a widget for visualizing surface data,
allowing for different implementations based on the type of surface
object.

Create a surfwidget to display brain surface data.

## Usage

``` r
surfwidget(x, width = NULL, height = NULL, ...)

# S4 method for class 'SurfaceGeometry'
surfwidget(
  x,
  width = NULL,
  height = NULL,
  data = NULL,
  cmap = grDevices::rainbow(256),
  irange = NULL,
  thresh = c(0, 0),
  vertexColors = NULL,
  alpha = 1,
  curvature = NULL,
  colorbar = TRUE,
  colorbar_label = NULL,
  layers = NULL,
  config = list(),
  ...
)

# S4 method for class 'NeuroSurface'
surfwidget(
  x,
  width = NULL,
  height = NULL,
  cmap = grDevices::rainbow(256),
  irange = range(x@data),
  thresh = c(0, 0),
  vertexColors = NULL,
  alpha = 1,
  curvature = NULL,
  colorbar = TRUE,
  colorbar_label = NULL,
  layers = NULL,
  config = list(),
  ...
)

# S4 method for class 'ColorMappedNeuroSurface'
surfwidget(
  x,
  width = NULL,
  height = NULL,
  thresh = c(0, 0),
  vertexColors = NULL,
  alpha = 1,
  curvature = NULL,
  colorbar = TRUE,
  colorbar_label = NULL,
  layers = NULL,
  config = list(),
  ...
)

# S4 method for class 'VertexColoredNeuroSurface'
surfwidget(
  x,
  width = NULL,
  height = NULL,
  alpha = 1,
  curvature = NULL,
  colorbar = TRUE,
  colorbar_label = NULL,
  layers = NULL,
  config = list(),
  ...
)
```

## Arguments

- x:

  A SurfaceGeometry, NeuroSurface, ColorMappedNeuroSurface, or
  VertexColoredNeuroSurface object

- width:

  The width of the widget

- height:

  The height of the widget

- ...:

  Additional arguments for customizing the widget appearance and
  behavior.

- data:

  Optional. Numeric vector of data values for each vertex.

- cmap:

  Optional. Color map for data visualization.

- irange:

  Optional. Intensity range for data visualization.

- thresh:

  Optional. Threshold range for data visualization.

- vertexColors:

  Optional. Vector of colors for each vertex.

- alpha:

  Opacity of the surface (0 to 1).

- curvature:

  Optional numeric vector of curvature values for each vertex. If not
  supplied for a `SurfaceGeometry` object, it is computed via
  `curvature(x)`.

- colorbar:

  Logical; if `TRUE` (default), render a colorbar when a colormap is
  used.

- colorbar_label:

  Optional character label shown alongside the colorbar.

- layers:

  Optional list of additional data layers to display on the surface.
  Each layer should be a list with elements such as `data`, `cmap`,
  `alpha`, and optionally outline-specific parameters.

- config:

  A list of configuration options for the surface rendering:

  `shininess`

  :   Numeric between 0 and 100. Controls the shininess of the material.
      Higher values create a more polished appearance. Default is 30.

  `specularColor`

  :   Character. Hex color code for the specular highlights. Default is
      "#111111".

  `flatShading`

  :   Logical scalar. If `TRUE`, uses flat shading; if `FALSE`, uses
      smooth shading. Default is `FALSE`.

  `ambientLightColor`

  :   Character. Hex color code for the ambient light. Default is
      "#404040".

  `directionalLightColor`

  :   Character. Hex color code for the directional light. Default is
      "#ffffff".

  `directionalLightIntensity`

  :   Numeric between 0 and 1. Intensity of the directional light.
      Default is 0.5.

  Unknown elements in `config` are ignored with a warning.

## Value

An HTMLWidget object representing the surface visualization.

An HTMLWidget object

## Details

The surfwidget function creates an interactive widget for visualizing
surface data, such as brain surfaces. The specific implementation
depends on the class of the object provided, allowing for customized
behavior for different types of surface representations.

## See also

[`plot_js`](plot_js-methods.md),
[`SurfaceGeometry`](SurfaceGeometry.md),
[`NeuroSurface`](NeuroSurface.md)
