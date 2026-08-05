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
  cmap = jet_colors(256),
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
  cmap = jet_colors(256),
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

# S4 method for class 'SurfaceScene'
surfwidget(x, width = NULL, height = NULL, config = list(), ...)
```

## Arguments

- x:

  A SurfaceScene, SurfaceGeometry, NeuroSurface,
  ColorMappedNeuroSurface, or VertexColoredNeuroSurface object.

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

[`plot_js`](https://bbuchsbaum.github.io/neurosurf/reference/plot_js-methods.md),
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry.md),
[`NeuroSurface`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface.md)

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
surfwidget(geom)

  Interactive left-hemisphere surface; map: data.


{"x":{"scene":{"schemaVersion":"surfview.scene.v1","id":"surface-scene","assets":{"vertices-sha256-c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af":{"id":"vertices-sha256-c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af","role":"vertices","dtype":"float32","shape":[4,3],"byteLength":48,"sha256":"c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af","endianness":"little","encoding":"base64","data":"AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAAAAAIA/"},"faces-sha256-d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae":{"id":"faces-sha256-d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae","role":"faces","dtype":"uint32","shape":[4,3],"byteLength":48,"sha256":"d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae","endianness":"little","encoding":"base64","data":"AAAAAAEAAAACAAAAAAAAAAEAAAADAAAAAAAAAAIAAAADAAAAAQAAAAIAAAADAAAA"},"curvature-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553":{"id":"curvature-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","role":"curvature","dtype":"float32","shape":[4],"byteLength":16,"sha256":"5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","endianness":"little","encoding":"base64","data":"AAAAAAAAgD8AAIA/AACAPw=="},"values-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553":{"id":"values-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","role":"values","dtype":"float32","shape":[4],"byteLength":16,"sha256":"5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","endianness":"little","encoding":"base64","data":"AAAAAAAAgD8AAIA/AACAPw=="},"indices-sha256-baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe":{"id":"indices-sha256-baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe","role":"indices","dtype":"uint32","shape":[4],"byteLength":16,"sha256":"baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe","endianness":"little","encoding":"base64","data":"AAAAAAEAAAACAAAAAwAAAA=="}},"geometries":{"left":{"id":"left","hemisphere":"left","vertices":"vertices-sha256-c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af","faces":"faces-sha256-d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae","curvature":"curvature-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","vertexCount":4,"faceCount":4,"metadata":{"label":null}}},"layers":{"data":{"id":"data","label":"data","values":{"left":{"values":"values-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","indices":"indices-sha256-baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe"}},"colorMap":["#00007F","#000083","#000087","#00008B","#00008F","#000093","#000097","#00009B","#00009F","#0000A3","#0000A7","#0000AB","#0000AF","#0000B3","#0000B7","#0000BB","#0000BF","#0000C3","#0000C7","#0000CB","#0000CF","#0000D3","#0000D7","#0000DB","#0000DF","#0000E3","#0000E7","#0000EB","#0000EF","#0000F3","#0000F7","#0000FB","#0000FF","#0004FF","#0008FF","#000CFF","#0010FF","#0014FF","#0018FF","#001CFF","#0020FF","#0024FF","#0028FF","#002CFF","#0030FF","#0034FF","#0038FF","#003CFF","#0040FF","#0044FF","#0048FF","#004CFF","#0050FF","#0054FF","#0058FF","#005CFF","#0060FF","#0064FF","#0068FF","#006CFF","#0070FF","#0074FF","#0078FF","#007CFF","#0080FF","#0084FF","#0088FF","#008CFF","#0090FF","#0094FF","#0098FF","#009CFF","#00A0FF","#00A4FF","#00A8FF","#00ACFF","#00B0FF","#00B4FF","#00B8FF","#00BCFF","#00C0FF","#00C4FF","#00C8FF","#00CCFF","#00D0FF","#00D4FF","#00D8FF","#00DCFF","#00E0FF","#00E4FF","#00E8FF","#00ECFF","#00F0FF","#00F4FF","#00F8FF","#00FCFF","#01FFFD","#05FFF9","#09FFF5","#0DFFF1","#11FFED","#15FFE9","#19FFE5","#1DFFE1","#21FFDD","#25FFD9","#29FFD5","#2DFFD1","#31FFCD","#35FFC9","#39FFC5","#3DFFC1","#41FFBD","#45FFB9","#49FFB5","#4DFFB1","#51FFAD","#55FFA9","#59FFA5","#5DFFA1","#61FF9D","#65FF99","#69FF95","#6DFF91","#71FF8D","#75FF89","#79FF85","#7DFF81","#81FF7D","#85FF79","#89FF75","#8DFF71","#91FF6D","#95FF69","#99FF65","#9DFF61","#A1FF5D","#A5FF59","#A9FF55","#ADFF51","#B1FF4D","#B5FF49","#B9FF45","#BDFF41","#C1FF3D","#C5FF39","#C9FF35","#CDFF31","#D1FF2D","#D5FF29","#D9FF25","#DDFF21","#E1FF1D","#E5FF19","#E9FF15","#EDFF11","#F1FF0D","#F5FF09","#F9FF05","#FDFF01","#FFFC00","#FFF800","#FFF400","#FFF000","#FFEC00","#FFE800","#FFE400","#FFE000","#FFDC00","#FFD800","#FFD400","#FFD000","#FFCC00","#FFC800","#FFC400","#FFC000","#FFBC00","#FFB800","#FFB400","#FFB000","#FFAC00","#FFA800","#FFA400","#FFA000","#FF9C00","#FF9800","#FF9400","#FF9000","#FF8C00","#FF8800","#FF8400","#FF8000","#FF7C00","#FF7800","#FF7400","#FF7000","#FF6C00","#FF6800","#FF6400","#FF6000","#FF5C00","#FF5800","#FF5400","#FF5000","#FF4C00","#FF4800","#FF4400","#FF4000","#FF3C00","#FF3800","#FF3400","#FF3000","#FF2C00","#FF2800","#FF2400","#FF2000","#FF1C00","#FF1800","#FF1400","#FF1000","#FF0C00","#FF0800","#FF0400","#FF0000","#FB0000","#F70000","#F30000","#EF0000","#EB0000","#E70000","#E30000","#DF0000","#DB0000","#D70000","#D30000","#CF0000","#CB0000","#C70000","#C30000","#BF0000","#BB0000","#B70000","#B30000","#AF0000","#AB0000","#A70000","#A30000","#9F0000","#9B0000","#970000","#930000","#8F0000","#8B0000","#870000","#830000","#7F0000"],"limits":[0,1],"threshold":[0,0],"opacity":1,"visible":true,"units":null,"legend":{"title":"data","visible":true}}},"selectedLayer":"data"},"options":{"lazy":true,"preset":"paper-light","controls":true,"mode":"report"},"config":[],"fallback":"Interactive left-hemisphere surface; map: data.","altText":"Interactive left-hemisphere cortical surface showing data.","calls":[]},"evals":[],"jsHooks":[]}# }
```
