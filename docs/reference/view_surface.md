# Display a 3D Brain Surface using RGL

Renders a 3D brain surface mesh using the \`rgl\` package. This function
provides flexible options for coloring the surface based on data values
or predefined colors, adjusting transparency, controlling lighting,
setting viewpoints, and overlaying spherical markers.

## Usage

``` r
view_surface(
  surfgeom,
  vals = NA,
  cmap = grDevices::rainbow(256, alpha = 1),
  vert_clrs = NULL,
  bgcol = "lightgray",
  alpha = 1,
  add_normals = TRUE,
  thresh = NULL,
  irange = NULL,
  specular = "black",
  lit = NULL,
  viewpoint = c("lateral", "medial", "ventral", "dorsal", "anterior", "posterior"),
  new_window = TRUE,
  offset = c(0, 0, 0),
  zoom = 1,
  spheres = NULL,
  spheres_map_surface = NULL,
  spheres_map_label = NULL,
  spheres_as_vertices = FALSE,
  vectors = NULL,
  vector_vertices = NULL,
  vector_scale = NULL,
  vector_color = "red",
  vector_alpha = 0.8,
  vector_lwd = 1.5,
  vals_vertices = NULL,
  vals_smoothing = c("auto", "nearest"),
  vals_smoothing_steps = 20,
  label = NULL,
  ...
)
```

## Arguments

- surfgeom:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the 3D brain surface mesh to be displayed, or a
  [`SurfaceSet`](SurfaceSet-class.md) containing multiple variants.

- vals:

  An optional numeric vector containing data values for each vertex on
  the surface. If provided and \`vert_clrs\` is NULL, these values are
  mapped to colors using \`cmap\` and \`irange\`.

- cmap:

  A vector of colors (e.g., hex codes) defining the color map used when
  \`vals\` is provided and \`vert_clrs\` is NULL. Defaults to
  \`rainbow(256)\`.

- vert_clrs:

  An optional character vector of hex color codes for each vertex. If
  provided, these colors directly override any coloring derived from
  \`vals\` and \`cmap\`. The length should match the number of vertices
  in \`surfgeom\`.

- bgcol:

  A single hex color code or a vector of hex color codes used as the
  base color for the surface. If \`vals\` or \`vert_clrs\` are provided,
  this color is blended with the data/vertex colors. Defaults to
  "lightgray".

- alpha:

  A numeric value between 0 (fully transparent) and 1 (fully opaque)
  controlling the overall transparency of the surface. Defaults to 1.

- add_normals:

  Logical. If TRUE (default), surface normals are calculated and added
  to the mesh, which improves the appearance of lighting effects.

- thresh:

  An optional numeric vector of length 2, \`c(lower, upper)\`. Vertices
  with \`vals\` \*outside\* this range (i.e., \`\< lower\` or \`\>
  upper\`) are made fully transparent. This is applied \*after\* the
  general \`alpha\`. Defaults to NULL (no thresholding).

- irange:

  An optional numeric vector of length 2, \`c(min, max)\`. Specifies the
  range of \`vals\` to map onto the \`cmap\`. Values outside this range
  will be clamped to the min/max colors. Defaults to the full range of
  \`vals\`.

- specular:

  The color of specular highlights on the surface, affecting its
  shininess. Can be a color name (e.g., "white") or hex code. Defaults
  to "black" for a matte look. Set to a brighter colour for a glossier
  appearance.

- lit:

  Logical. If `TRUE`, enables lighting effects on the surface. If
  `FALSE`, disables lighting for a flat appearance. If `NULL` (default),
  automatically sets to `TRUE` for interactive sessions and `FALSE` when
  knitting (when `rgl.useNULL` is `TRUE`).

- viewpoint:

  A character string specifying a predefined view (e.g., "lateral",
  "medial", "ventral", "dorsal", "anterior", "posterior"). The actual
  view depends on the hemisphere (\`surfgeom@hemi\`, e.g.,
  "left_lateral"). Alternatively, a 4x4 transformation matrix defining a
  custom view. Defaults to "lateral".

- new_window:

  Logical. If TRUE (default), opens a new \`rgl\` window for the plot.
  If FALSE, attempts to plot in the currently active \`rgl\` window
  (useful for updates or within Shiny apps).

- offset:

  A numeric vector of length 3 specifying a translation offset \`c(x, y,
  z)\` applied to the surface coordinates before rendering. Defaults to
  \`c(0, 0, 0)\`.

- zoom:

  A numeric value controlling the camera zoom level. Defaults to 1 (no
  zoom). Values \> 1 zoom in, \< 1 zoom out.

- spheres:

  An optional data frame to draw spheres at specific locations on or
  near the surface. Must contain columns \`x\`, \`y\`, \`z\`
  (coordinates), and \`radius\`. Can optionally include a \`color\`
  column (hex codes or color names) for individual sphere colors
  (defaults to black). Alternatively, supply a \`vertex\` column
  (1-based vertex ids) and set `spheres_as_vertices = TRUE` to position
  foci by vertex.

- spheres_map_surface:

  Optional `SurfaceGeometry`, `SurfaceSet`, or file path used to map
  sphere coordinates to the nearest vertex on that surface before
  snapping to `surfgeom`. Assumes both surfaces share the same vertex
  ordering (e.g., white -\> inflated).

- spheres_map_label:

  Optional surface label to use when `spheres_map_surface` is a
  `SurfaceSet`.

- spheres_as_vertices:

  Logical; if `TRUE`, interpret the \`vertex\` column of `spheres` as
  1-based vertex ids on `surfgeom` rather than raw coordinates.

- vectors:

  Optional matrix (n x 3) of XYZ vectors to draw as line glyphs.

- vector_vertices:

  Optional vertex ids matching rows of `vectors` when they are defined
  on a subset of vertices.

- vector_scale:

  Optional numeric scale factor for vectors. If `NULL`, a heuristic
  scale based on mesh extent and vector magnitudes is used.

- vector_color:

  Colour for the vectors (single value or vector).

- vector_alpha:

  Opacity for the vectors (0–1).

- vector_lwd:

  Numeric line width for vector glyphs.

- vals_vertices:

  Optional integer vector of 1-based vertex ids corresponding to
  \`vals\` when \`length(vals) \< n_vertices\`. Enables sparse data
  inputs.

- vals_smoothing:

  One of \`"auto"\` (default) or \`"nearest"\`. When using sparse data,
  \`"auto"\` diffuses values with neighbor averaging after nearest fill;
  \`"nearest"\` performs nearest-neighbour fill only.

- vals_smoothing_steps:

  Integer number of smoothing iterations applied when \`vals_smoothing =
  "auto"\`. Ignored otherwise.

- label:

  Optional surface label to select when \`surfgeom\` is a `SurfaceSet`.
  Defaults to the set's \`default_label\`.

- ...:

  Additional arguments passed directly to \`rgl::shade3d\` for
  fine-grained control over rendering (e.g., \`lit\`, \`smooth\`).

## Value

Invisibly returns the object ID(s) of the shape(s) added to the RGL
scene by \`rgl::shade3d\`. This can be useful for modifying the scene
later.

## Details

\*\*Coloring:\*\* Surface vertex colors are determined by the following
priority: 1. \`vert_clrs\`: If provided, these specific hex colors are
used. 2. \`vals\` & \`cmap\`: If \`vals\` is provided and \`vert_clrs\`
is NULL, \`vals\` are mapped to \`cmap\` based on \`irange\`. 3.
\`bgcol\`: If neither \`vert_clrs\` nor \`vals\` are used for coloring,
\`bgcol\` is applied uniformly. If \`bgcol\` is specified alongside
\`vert_clrs\` or \`vals\`, the colors are blended based on the \`alpha\`
parameter.

\*\*Transparency:\*\* Overall transparency is set by \`alpha\`.
Additional threshold-based transparency can be applied using \`thresh\`
when \`vals\` are provided. Vertices with values outside the \`thresh\`
range become fully transparent.

\*\*Lighting:\*\* \`add_normals=TRUE\` is recommended for realistic
lighting. The \`specular\` parameter controls the shininess.

\*\*Viewpoint:\*\* Predefined viewpoints (\`"lateral"\`, \`"medial"\`,
etc.) are automatically adjusted based on the hemisphere specified in
\`surfgeom@hemi\` (e.g., "lh" results in "left_lateral"). If \`hemi\` is
unknown, the current \`rgl\` view is used unless a custom 4x4 matrix is
provided.

\*\*Performance:\*\* Rendering very large surfaces or surfaces with
complex coloring/transparency can be computationally intensive.

## See also

[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html),
[`spheres3d`](https://dmurdoch.github.io/rgl/dev/reference/spheres.html),
[`view3d`](https://dmurdoch.github.io/rgl/dev/reference/viewpoint.html),
[`SurfaceGeometry`](SurfaceGeometry.md)

## Examples

``` r
# \donttest{
surf_geom <- example_surface_geometry()
view_surface(surf_geom, viewpoint = "lateral")
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
3D plot

{"x":{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":true,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmode":"modulate","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false,"polygon_offset":[0,0],"margin":"","floating":false,"tag":"","blend":["src_alpha","one_minus_src_alpha"]},"rootSubscene":6,"objects":{"302":{"id":302,"type":"triangles","material":{"lit":false,"specular":"#000000","polygon_offset":[1,1]},"vertices":"0","colors":"2","centers":"3","normals":"1","ignoreExtent":false,"flags":32770},"301":{"id":301,"type":"background","material":{"lit":false,"back":"lines"},"colors":"4","centers":"5","sphere":false,"fogtype":"none","fogscale":1,"flags":32768},"6":{"id":6,"type":"subscene","par3d":{"antialias":8,"FOV":0,"ignoreExtent":false,"listeners":6,"mouseMode":{"none":"none","left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,1.732050776481628],"modelMatrix":[[0,-1,0,0.5],[0,0,1,-0.5],[-1,0,0,-1.232050776481628],[0,0,0,1]],"projMatrix":[[1.154700517654419,0,0,0],[0,1.154700517654419,0,0],[0,0,-1.154700636863708,-2.000000238418579],[0,0,0,1]],"skipRedraw":false,"userMatrix":[[0,-1,0,0],[0,0,1,0],[-1,0,0,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[0,1,0,1,0,1],"windowRect":[0,0,256,256],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"NULL","maxClipPlanes":2147483647,"glVersion":"NA","activeSubscene":0},"embeddings":{"viewport":"replace","projection":"replace","model":"replace","mouse":"replace"},"objects":[301,302],"subscenes":[],"flags":33026}},"crosstalk":{"key":[],"group":[],"id":[],"options":[]},"width":700,"height":432.6328800988875,"buffer":{"accessors":[{"bufferView":0,"componentType":5121,"count":12,"type":"VEC3"},{"bufferView":1,"componentType":5126,"count":12,"type":"VEC3"},{"bufferView":2,"componentType":5121,"count":12,"type":"VEC4","normalized":true},{"bufferView":3,"componentType":5126,"count":4,"type":"VEC3"},{"bufferView":4,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":5,"componentType":5121,"count":1,"type":"VEC3"}],"bufferViews":[{"buffer":0,"byteLength":36,"byteOffset":0},{"buffer":0,"byteLength":144,"byteOffset":36},{"buffer":0,"byteLength":48,"byteOffset":180},{"buffer":0,"byteLength":48,"byteOffset":228},{"buffer":0,"byteLength":4,"byteOffset":276},{"buffer":0,"byteLength":3,"byteOffset":280}],"buffers":[{"byteLength":283,"bytes":"AAAAAQAAAAEAAAAAAQAAAAABAAAAAAEAAAABAQAAAAEAAAABOs0TPzrNE786zRM/P8jKPvaO\n8r0eGmk/RgUtPwKElj5GBS0/Os0TPzrNE786zRM/P8jKPvaO8r0eGmk/HhppP/aO8r0/yMo+\nOs0TPzrNE786zRM/RgUtPwKElj5GBS0/HhppP/aO8r0/yMo+P8jKPvaO8r0eGmk/RgUtPwKE\nlj5GBS0/HhppP/aO8r0/yMo+09PT/9PT0//T09P/09PT/9PT0//T09P/09PT/9PT0//T09P/\n09PT/9PT0//T09P/q6qqPquqqj4AAAAAq6qqPgAAAACrqqo+AAAAAKuqqj6rqqo+q6qqPquq\nqj6rqqo+AQEBAQAAAA=="}]},"context":{"shiny":false,"rmarkdown":null},"vertexShader":"#line 2 1\n// File 1 is the vertex shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\n\nattribute vec3 aPos;\nattribute vec4 aCol;\nuniform mat4 mvMatrix;\nuniform mat4 prMatrix;\nvarying vec4 vCol;\nvarying vec4 vPosition;\n\n#ifdef NEEDS_VNORMAL\nattribute vec3 aNorm;\nuniform mat4 normMatrix;\nvarying vec4 vNormal;\n#endif\n\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nattribute vec2 aTexcoord;\nvarying vec2 vTexcoord;\n#endif\n\n#ifdef FIXED_SIZE\nuniform vec3 textScale;\n#endif\n\n#ifdef FIXED_QUADS\nattribute vec3 aOfs;\n#endif\n\n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\nvarying float normz;\nuniform mat4 invPrMatrix;\n#else\nattribute vec3 aPos1;\nattribute vec3 aPos2;\nvarying float normz;\n#endif\n#endif // IS_TWOSIDED\n\n#ifdef FAT_LINES\nattribute vec3 aNext;\nattribute vec2 aPoint;\nvarying vec2 vPoint;\nvarying float vLength;\nuniform float uAspect;\nuniform float uLwd;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  \n#ifndef IS_BRUSH\n#if defined(NCLIPPLANES) || !defined(FIXED_QUADS) || defined(HAS_FOG) || defined(USE_ENVMAP)\n  vPosition = mvMatrix * vec4(aPos, 1.);\n#endif\n  \n#ifndef FIXED_QUADS\n  gl_Position = prMatrix * vPosition;\n#endif\n#endif // !IS_BRUSH\n  \n#ifdef IS_POINTS\n  gl_PointSize = POINTSIZE;\n#endif\n  \n  vCol = aCol;\n  \n// USE_ENVMAP implies NEEDS_VNORMAL\n\n#ifdef NEEDS_VNORMAL\n  vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));\n#endif\n\n#ifdef USE_ENVMAP\n  vReflection = normalize(reflect(vPosition.xyz/vPosition.w, \n                        normalize(vNormal.xyz/vNormal.w)));\n#endif\n  \n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\n  /* normz should be calculated *after* projection */\n  normz = (invPrMatrix*vNormal).z;\n#else\n  vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n  pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n  vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n  pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n  normz = pos1.x*pos2.y - pos1.y*pos2.x;\n#endif\n#endif // IS_TWOSIDED\n  \n#ifdef NEEDS_VNORMAL\n  vNormal = vec4(normalize(vNormal.xyz), 1);\n#endif\n  \n#if defined(HAS_TEXTURE) || defined(IS_TEXT)\n  vTexcoord = aTexcoord;\n#endif\n  \n#if defined(FIXED_SIZE) && !defined(ROTATING)\n  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w;\n  gl_Position = pos + vec4(aOfs*textScale, 0.);\n#endif\n  \n#if defined(IS_SPRITES) && !defined(FIXED_SIZE)\n  vec4 pos = mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w + vec4(aOfs,  0.);\n  gl_Position = prMatrix*pos;\n#endif\n  \n#ifdef FAT_LINES\n  /* This code was inspired by Matt Deslauriers' code in \n   https://mattdesl.svbtle.com/drawing-lines-is-hard */\n  vec2 aspectVec = vec2(uAspect, 1.0);\n  mat4 projViewModel = prMatrix * mvMatrix;\n  vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n  currentProjected = currentProjected/currentProjected.w;\n  vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n  vec2 currentScreen = currentProjected.xy * aspectVec;\n  vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n  float len = uLwd;\n  vec2 dir = vec2(1.0, 0.0);\n  vPoint = aPoint;\n  vLength = length(nextScreen - currentScreen)/2.0;\n  vLength = vLength/(vLength + len);\n  if (vLength > 0.0) {\n    dir = normalize(nextScreen - currentScreen);\n  }\n  vec2 normal = vec2(-dir.y, dir.x);\n  dir.x /= uAspect;\n  normal.x /= uAspect;\n  vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n  gl_Position = currentProjected + offset;\n#endif\n  \n#ifdef IS_BRUSH\n  gl_Position = vec4(aPos, 1.);\n#endif\n}","fragmentShader":"#line 2 2\n// File 2 is the fragment shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\nvarying vec4 vCol; // carries alpha\nvarying vec4 vPosition;\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nvarying vec2 vTexcoord;\nuniform sampler2D uSampler;\n#endif\n\n#ifdef HAS_FOG\nuniform int uFogMode;\nuniform vec3 uFogColor;\nuniform vec4 uFogParms;\n#endif\n\n#if defined(IS_LIT) && !defined(FIXED_QUADS)\nvarying vec4 vNormal;\n#endif\n\n#if NCLIPPLANES > 0\nuniform vec4 vClipplane[NCLIPPLANES];\n#endif\n\n#if NLIGHTS > 0\nuniform mat4 mvMatrix;\n#endif\n\n#ifdef IS_LIT\nuniform vec3 emission;\nuniform float shininess;\n#if NLIGHTS > 0\nuniform vec3 ambient[NLIGHTS];\nuniform vec3 specular[NLIGHTS]; // light*material\nuniform vec3 diffuse[NLIGHTS];\nuniform vec3 lightDir[NLIGHTS];\nuniform bool viewpoint[NLIGHTS];\nuniform bool finite[NLIGHTS];\n#endif\n#endif // IS_LIT\n\n#ifdef IS_TWOSIDED\nuniform bool front;\nvarying float normz;\n#endif\n\n#ifdef FAT_LINES\nvarying vec2 vPoint;\nvarying float vLength;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  vec4 fragColor;\n#ifdef FAT_LINES\n  vec2 point = vPoint;\n  bool neg = point.y < 0.0;\n  point.y = neg ? (point.y + vLength)/(1.0 - vLength) :\n                 -(point.y - vLength)/(1.0 - vLength);\n#if defined(IS_TRANSPARENT) && defined(IS_LINESTRIP)\n  if (neg && length(point) <= 1.0) discard;\n#endif\n  point.y = min(point.y, 0.0);\n  if (length(point) > 1.0) discard;\n#endif // FAT_LINES\n  \n#ifdef ROUND_POINTS\n  vec2 coord = gl_PointCoord - vec2(0.5);\n  if (length(coord) > 0.5) discard;\n#endif\n  \n#if NCLIPPLANES > 0\n  for (int i = 0; i < NCLIPPLANES; i++)\n    if (dot(vPosition, vClipplane[i]) < 0.0) discard;\n#endif\n    \n#ifdef FIXED_QUADS\n    vec3 n = vec3(0., 0., 1.);\n#elif defined(IS_LIT)\n    vec3 n = normalize(vNormal.xyz);\n#endif\n    \n#ifdef IS_TWOSIDED\n    if ((normz <= 0.) != front) discard;\n#endif\n\n#ifdef IS_LIT\n    vec3 eye = normalize(-vPosition.xyz/vPosition.w);\n    vec3 lightdir;\n    vec4 colDiff;\n    vec3 halfVec;\n    vec4 lighteffect = vec4(emission, 0.);\n    vec3 col;\n    float nDotL;\n#ifdef FIXED_QUADS\n    n = -faceforward(n, n, eye);\n#endif\n    \n#if NLIGHTS > 0\n    // Simulate two-sided lighting\n    if (n.z < 0.0)\n      n = -n;\n    for (int i=0;i<NLIGHTS;i++) {\n      colDiff = vec4(vCol.rgb * diffuse[i], vCol.a);\n      lightdir = lightDir[i];\n      if (!viewpoint[i]) {\n        if (finite[i]) {\n          lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n        } else {\n          lightdir = (mvMatrix * vec4(lightdir, 0.)).xyz;\n        }\n      }\n      if (!finite[i]) {\n        halfVec = normalize(lightdir + eye);\n      } else {\n        lightdir = normalize(lightdir - vPosition.xyz/vPosition.w);\n        halfVec = normalize(lightdir + eye);\n      }\n      col = ambient[i];\n      nDotL = dot(n, lightdir);\n      col = col + max(nDotL, 0.) * colDiff.rgb;\n      col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular[i];\n      lighteffect = lighteffect + vec4(col, colDiff.a);\n    }\n#else\n    lighteffect.a = 1.;\n#endif\n    \n#else // not IS_LIT\n    vec4 colDiff = vCol;\n    vec4 lighteffect = colDiff;\n#endif\n    \n#ifdef IS_TEXT\n    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n#endif\n    \n#ifdef HAS_TEXTURE\n\n// These calculations use the definitions from \n// https://docs.gl/gl3/glTexEnv\n\n#ifdef USE_ENVMAP\n    float m = 2.0 * sqrt(dot(vReflection, vReflection) + 2.0*vReflection.z + 1.0);\n    vec4 textureColor = texture2D(uSampler, vReflection.xy / m + vec2(0.5, 0.5));\n#else\n    vec4 textureColor = texture2D(uSampler, vTexcoord);\n#endif\n\n#ifdef TEXTURE_rgb\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(textureColor.rgb, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*vec4(textureColor.rgb, 1.);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb, lighteffect.a);\n#endif\n\n#endif //TEXTURE_rgb\n        \n#ifdef TEXTURE_rgba\n\n#ifdef TEXMODE_replace\n// already done\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*textureColor;\n#endif\n\n#ifdef TEXMODE_decal\n    textureColor = vec4((1. - textureColor.a)*lighteffect.rgb) +\n                     textureColor.a*textureColor.rgb, \n                     lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n    \n#endif //TEXTURE_rgba\n    \n#ifdef TEXTURE_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(lighteffect.rgb, luminance);\n#endif \n\n#if defined(TEXMODE_modulate) || defined(TEXMODE_blend) || defined(TEXMODE_add)\n    textureColor = vec4(lighteffect.rgb, lighteffect.a*luminance);\n#endif\n \n#endif // TEXTURE_alpha\n    \n// The TEXTURE_luminance values are not from that reference    \n#ifdef TEXTURE_luminance\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, lighteffect.a);\n#endif\n\n#endif // TEXTURE_luminance\n \n    \n#ifdef TEXTURE_luminance_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, textureColor.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n\n#endif\n\n#endif // TEXTURE_luminance_alpha\n    \n    fragColor = textureColor;\n\n#elif defined(IS_TEXT)\n    if (textureColor.a < 0.1)\n      discard;\n    else\n      fragColor = textureColor;\n#else\n    fragColor = lighteffect;\n#endif // HAS_TEXTURE\n    \n#ifdef HAS_FOG\n    // uFogParms elements: x = near, y = far, z = fogscale, w = (1-sin(FOV/2))/(1+sin(FOV/2))\n    // In Exp and Exp2: use density = density/far\n    // fogF will be the proportion of fog\n    // Initialize it to the linear value\n    float fogF;\n    if (uFogMode > 0) {\n      fogF = (uFogParms.y - vPosition.z/vPosition.w)/(uFogParms.y - uFogParms.x);\n      if (uFogMode > 1)\n        fogF = mix(uFogParms.w, 1.0, fogF);\n      fogF = fogF*uFogParms.z;\n      if (uFogMode == 2)\n        fogF = 1.0 - exp(-fogF);\n      // Docs are wrong: use (density*c)^2, not density*c^2\n      // https://gitlab.freedesktop.org/mesa/mesa/-/blob/master/src/mesa/swrast/s_fog.c#L58\n      else if (uFogMode == 3)\n        fogF = 1.0 - exp(-fogF*fogF);\n      fogF = clamp(fogF, 0.0, 1.0);\n      gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);\n    } else gl_FragColor = fragColor;\n#else\n    gl_FragColor = fragColor;\n#endif // HAS_FOG\n    \n}","players":[],"webGLoptions":{"preserveDrawingBuffer":true},"fastTransparency":true},"evals":[],"jsHooks":[]}# }
```
