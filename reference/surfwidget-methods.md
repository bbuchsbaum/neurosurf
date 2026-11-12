# Create an interactive surface viewer

Create an HTML widget from a portable `SurfaceScene` or adapt a legacy
single-surface object to one. Use `SurfaceScene` for bilateral viewers,
multiple named maps, authored fallbacks, and portable report assets.

## Usage

``` r
surfwidget(x, width = NULL, height = NULL, ...)

# S4 method for class 'SurfaceGeometry'
surfwidget(
  x,
  width = NULL,
  height = NULL,
  data = NULL,
  cmap = NULL,
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
  cmap = NULL,
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

  A `SurfaceScene`, `SurfaceGeometry`, `NeuroSurface`,
  `ColorMappedNeuroSurface`, or `VertexColoredNeuroSurface`.

- width:

  Optional widget width.

- height:

  Optional widget height.

- ...:

  Arguments passed to the method for `x`.

- data:

  Optional. Numeric vector of data values for each vertex.

- cmap:

  Optional colour map. The default, `NULL`, uses
  [`surface_palette`](https://bbuchsbaum.github.io/neurosurf/reference/surface_palette.md)
  (diverging when the range spans zero), or greys when a
  `SurfaceGeometry` is shown without data.

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

  Legacy list of additional single-surface data layers. New code should
  pass
  [`surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md)
  objects to
  [`surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md).

- config:

  Configuration for legacy single-surface calls:

  `preset`

  :   Appearance preset used while adapting the object. The default is
      `"paper-light"`.

  `mode`

  :   `"report"` for the curated report toolbar or `"viewer"` for a bare
      viewer.

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

  Unknown elements are ignored with a warning. `showControls` and
  `controlType` are deprecated; report widgets do not load Tweakpane.

## Value

An `htmlwidget` containing the local surfview runtime and scene.

## Details

The browser renders the values supplied by R. Perform statistical
thresholding, capping, atlas projection, and other scientific
transformations before constructing the widget. The legacy
single-surface methods create a one-hemisphere `SurfaceScene`; they do
not expose the full bilateral and portable-scene contract.

`preset = "paper-light"` controls appearance. `mode = "report"` controls
behavior and enables the compact map, anatomical-view, reset, and PNG
controls. Set these fields on
[`surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md)
for an explicit scene.

## See also

[`surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md),
[`surface_layer`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md),
[`write_surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md),
[`plot_js`](https://bbuchsbaum.github.io/neurosurf/reference/plot_js-methods.md)

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
surfwidget(geom)

  Interactive left-hemisphere surface; map: data.


{"x":{"scene":{"schemaVersion":"surfview.scene.v1","id":"surface-scene","assets":{"vertices-sha256-c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af":{"id":"vertices-sha256-c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af","role":"vertices","dtype":"float32","shape":[4,3],"byteLength":48,"sha256":"c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af","endianness":"little","encoding":"base64","data":"AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAAAAAIA/"},"faces-sha256-d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae":{"id":"faces-sha256-d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae","role":"faces","dtype":"uint32","shape":[4,3],"byteLength":48,"sha256":"d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae","endianness":"little","encoding":"base64","data":"AAAAAAEAAAACAAAAAAAAAAEAAAADAAAAAAAAAAIAAAADAAAAAQAAAAIAAAADAAAA"},"curvature-sha256-f7015af0491af7482f01573000c48ca03f016c5fb735e4833f352e937ddc9e2a":{"id":"curvature-sha256-f7015af0491af7482f01573000c48ca03f016c5fb735e4833f352e937ddc9e2a","role":"curvature","dtype":"float32","shape":[4],"byteLength":16,"sha256":"f7015af0491af7482f01573000c48ca03f016c5fb735e4833f352e937ddc9e2a","endianness":"little","encoding":"base64","data":"AAAAgAAAgL8AAIC/AACAvw=="},"values-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553":{"id":"values-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","role":"values","dtype":"float32","shape":[4],"byteLength":16,"sha256":"5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","endianness":"little","encoding":"base64","data":"AAAAAAAAgD8AAIA/AACAPw=="},"indices-sha256-baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe":{"id":"indices-sha256-baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe","role":"indices","dtype":"uint32","shape":[4],"byteLength":16,"sha256":"baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe","endianness":"little","encoding":"base64","data":"AAAAAAEAAAACAAAAAwAAAA=="}},"geometries":{"left":{"id":"left","hemisphere":"left","vertices":"vertices-sha256-c41861748509cec7903cf9025b773179349bc8ca13aebccaba898d76c46b12af","faces":"faces-sha256-d208dd4e199c9a16be7742848a5c7f59c53841fc75efe420d77ec251603b26ae","curvature":"curvature-sha256-f7015af0491af7482f01573000c48ca03f016c5fb735e4833f352e937ddc9e2a","vertexCount":4,"faceCount":4,"metadata":{"label":null}}},"layers":{"data":{"id":"data","role":"map","label":"data","values":{"left":{"values":"values-sha256-5437fe318884c7cf51ef423bb15a63e327a4305f4be707e6a9ec2699dcff6553","indices":"indices-sha256-baed642339816affb3fe8719792d0e4ce82f12db72b7373d244eaa65445800fe"}},"colorMap":["#4D4D4D","#4E4E4E","#4F4F4F","#515151","#525252","#535353","#545454","#565656","#575757","#585858","#595959","#5A5A5A","#5B5B5B","#5D5D5D","#5E5E5E","#5F5F5F","#606060","#616161","#626262","#636363","#646464","#656565","#666666","#676767","#686868","#696969","#6A6A6A","#6B6B6B","#6C6C6C","#6D6D6D","#6E6E6E","#6E6E6E","#6F6F6F","#707070","#717171","#727272","#737373","#747474","#747474","#757575","#767676","#777777","#787878","#797979","#797979","#7A7A7A","#7B7B7B","#7C7C7C","#7D7D7D","#7D7D7D","#7E7E7E","#7F7F7F","#808080","#808080","#818181","#828282","#838383","#838383","#848484","#858585","#858585","#868686","#878787","#888888","#888888","#898989","#8A8A8A","#8A8A8A","#8B8B8B","#8C8C8C","#8C8C8C","#8D8D8D","#8E8E8E","#8E8E8E","#8F8F8F","#909090","#909090","#919191","#929292","#929292","#939393","#949494","#949494","#959595","#959595","#969696","#979797","#979797","#989898","#999999","#999999","#9A9A9A","#9A9A9A","#9B9B9B","#9C9C9C","#9C9C9C","#9D9D9D","#9D9D9D","#9E9E9E","#9E9E9E","#9F9F9F","#A0A0A0","#A0A0A0","#A1A1A1","#A1A1A1","#A2A2A2","#A2A2A2","#A3A3A3","#A4A4A4","#A4A4A4","#A5A5A5","#A5A5A5","#A6A6A6","#A6A6A6","#A7A7A7","#A7A7A7","#A8A8A8","#A9A9A9","#A9A9A9","#AAAAAA","#AAAAAA","#ABABAB","#ABABAB","#ACACAC","#ACACAC","#ADADAD","#ADADAD","#AEAEAE","#AEAEAE","#AFAFAF","#AFAFAF","#B0B0B0","#B0B0B0","#B1B1B1","#B1B1B1","#B2B2B2","#B2B2B2","#B3B3B3","#B3B3B3","#B4B4B4","#B4B4B4","#B5B5B5","#B5B5B5","#B6B6B6","#B6B6B6","#B7B7B7","#B7B7B7","#B8B8B8","#B8B8B8","#B9B9B9","#B9B9B9","#BABABA","#BABABA","#BBBBBB","#BBBBBB","#BCBCBC","#BCBCBC","#BDBDBD","#BDBDBD","#BEBEBE","#BEBEBE","#BFBFBF","#BFBFBF","#BFBFBF","#C0C0C0","#C0C0C0","#C1C1C1","#C1C1C1","#C2C2C2","#C2C2C2","#C3C3C3","#C3C3C3","#C4C4C4","#C4C4C4","#C5C5C5","#C5C5C5","#C5C5C5","#C6C6C6","#C6C6C6","#C7C7C7","#C7C7C7","#C8C8C8","#C8C8C8","#C8C8C8","#C9C9C9","#C9C9C9","#CACACA","#CACACA","#CBCBCB","#CBCBCB","#CCCCCC","#CCCCCC","#CCCCCC","#CDCDCD","#CDCDCD","#CECECE","#CECECE","#CFCFCF","#CFCFCF","#CFCFCF","#D0D0D0","#D0D0D0","#D1D1D1","#D1D1D1","#D1D1D1","#D2D2D2","#D2D2D2","#D3D3D3","#D3D3D3","#D4D4D4","#D4D4D4","#D4D4D4","#D5D5D5","#D5D5D5","#D6D6D6","#D6D6D6","#D6D6D6","#D7D7D7","#D7D7D7","#D8D8D8","#D8D8D8","#D8D8D8","#D9D9D9","#D9D9D9","#DADADA","#DADADA","#DADADA","#DBDBDB","#DBDBDB","#DCDCDC","#DCDCDC","#DCDCDC","#DDDDDD","#DDDDDD","#DEDEDE","#DEDEDE","#DEDEDE","#DFDFDF","#DFDFDF","#DFDFDF","#E0E0E0","#E0E0E0","#E1E1E1","#E1E1E1","#E1E1E1","#E2E2E2","#E2E2E2","#E2E2E2","#E3E3E3","#E3E3E3","#E4E4E4","#E4E4E4","#E4E4E4","#E5E5E5","#E5E5E5","#E6E6E6"],"limits":[0,1],"threshold":[0,0],"opacity":1,"visible":true,"units":null,"legend":{"title":"data","visible":true}}},"selectedLayer":"data"},"options":{"lazy":true,"preset":"paper-light","controls":true,"mode":"report"},"config":[],"fallback":"Interactive left-hemisphere surface; map: data.","altText":"Interactive left-hemisphere cortical surface showing data.","initialView":"lateral","calls":[]},"evals":[],"jsHooks":[]}# }
```
