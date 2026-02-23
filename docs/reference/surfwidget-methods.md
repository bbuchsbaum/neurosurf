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
  thresh = NULL,
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

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
surfwidget(geom)

{"x":{"vertices":[0,0,0,1,0,0,0,1,0,0,0,1],"faces":[0,1,2,0,1,3,0,2,3,1,2,3],"hemi":"lh","data":[0,1,1,1],"indices":[0,1,2,3],"thresh":[0,0],"alpha":1,"cmap":["#FF0000","#FF0600","#FF0C00","#FF1200","#FF1800","#FF1E00","#FF2400","#FF2A00","#FF3000","#FF3600","#FF3C00","#FF4200","#FF4800","#FF4E00","#FF5400","#FF5A00","#FF6000","#FF6600","#FF6C00","#FF7200","#FF7800","#FF7E00","#FF8300","#FF8900","#FF8F00","#FF9500","#FF9B00","#FFA100","#FFA700","#FFAD00","#FFB300","#FFB900","#FFBF00","#FFC500","#FFCB00","#FFD100","#FFD700","#FFDD00","#FFE300","#FFE900","#FFEF00","#FFF500","#FFFB00","#FDFF00","#F7FF00","#F1FF00","#EBFF00","#E5FF00","#DFFF00","#D9FF00","#D3FF00","#CDFF00","#C7FF00","#C1FF00","#BBFF00","#B5FF00","#AFFF00","#A9FF00","#A3FF00","#9DFF00","#97FF00","#91FF00","#8BFF00","#85FF00","#80FF00","#7AFF00","#74FF00","#6EFF00","#68FF00","#62FF00","#5CFF00","#56FF00","#50FF00","#4AFF00","#44FF00","#3EFF00","#38FF00","#32FF00","#2CFF00","#26FF00","#20FF00","#1AFF00","#14FF00","#0EFF00","#08FF00","#02FF00","#00FF04","#00FF0A","#00FF10","#00FF16","#00FF1C","#00FF22","#00FF28","#00FF2E","#00FF34","#00FF3A","#00FF40","#00FF46","#00FF4C","#00FF52","#00FF58","#00FF5E","#00FF64","#00FF6A","#00FF70","#00FF76","#00FF7C","#00FF81","#00FF87","#00FF8D","#00FF93","#00FF99","#00FF9F","#00FFA5","#00FFAB","#00FFB1","#00FFB7","#00FFBD","#00FFC3","#00FFC9","#00FFCF","#00FFD5","#00FFDB","#00FFE1","#00FFE7","#00FFED","#00FFF3","#00FFF9","#00FFFF","#00F9FF","#00F3FF","#00EDFF","#00E7FF","#00E1FF","#00DBFF","#00D5FF","#00CFFF","#00C9FF","#00C3FF","#00BDFF","#00B7FF","#00B1FF","#00ABFF","#00A5FF","#009FFF","#0099FF","#0093FF","#008DFF","#0087FF","#0081FF","#007CFF","#0076FF","#0070FF","#006AFF","#0064FF","#005EFF","#0058FF","#0052FF","#004CFF","#0046FF","#0040FF","#003AFF","#0034FF","#002EFF","#0028FF","#0022FF","#001CFF","#0016FF","#0010FF","#000AFF","#0004FF","#0200FF","#0800FF","#0E00FF","#1400FF","#1A00FF","#2000FF","#2600FF","#2C00FF","#3200FF","#3800FF","#3E00FF","#4400FF","#4A00FF","#5000FF","#5600FF","#5C00FF","#6200FF","#6800FF","#6E00FF","#7400FF","#7A00FF","#8000FF","#8500FF","#8B00FF","#9100FF","#9700FF","#9D00FF","#A300FF","#A900FF","#AF00FF","#B500FF","#BB00FF","#C100FF","#C700FF","#CD00FF","#D300FF","#D900FF","#DF00FF","#E500FF","#EB00FF","#F100FF","#F700FF","#FD00FF","#FF00FB","#FF00F5","#FF00EF","#FF00E9","#FF00E3","#FF00DD","#FF00D7","#FF00D1","#FF00CB","#FF00C5","#FF00BF","#FF00B9","#FF00B3","#FF00AD","#FF00A7","#FF00A1","#FF009B","#FF0095","#FF008F","#FF0089","#FF0083","#FF007E","#FF0078","#FF0072","#FF006C","#FF0066","#FF0060","#FF005A","#FF0054","#FF004E","#FF0048","#FF0042","#FF003C","#FF0036","#FF0030","#FF002A","#FF0024","#FF001E","#FF0018","#FF0012","#FF000C","#FF0006"],"irange":[0,1],"curv":[0,1,1,1],"config":[],"colorbar":{"show":true,"label":null}},"evals":[],"jsHooks":[]}# }
```
