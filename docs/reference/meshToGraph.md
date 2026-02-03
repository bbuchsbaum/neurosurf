# Construct a Graph from Mesh Vertices and Faces

This function creates an igraph object representing the connectivity
structure of a 3D mesh based on its vertices and triangular faces.

## Usage

``` r
meshToGraph(vertices, nodes)
```

## Arguments

- vertices:

  A numeric matrix with 3 columns representing the x, y, and z
  coordinates of vertices. Each row corresponds to a vertex.

- nodes:

  A numeric matrix where each row represents a triangular face,
  containing 0-based indices of three vertices that form the face.

## Value

An `igraph` object representing the mesh connectivity. The graph has the
following attributes:

- Vertex attributes: "x", "y", and "z" coordinates from the vertices
  matrix

- Edge attribute: "dist" (Euclidean distance between connected vertices)

## Details

The function converts a triangular mesh into a graph representation
where:

- Vertices of the graph correspond to vertices of the mesh

- Edges of the graph correspond to the edges of triangular faces in the
  mesh

The function performs the following steps:

1.  Extracts all unique edges from the triangular faces

2.  Creates an undirected graph from these edges

3.  Simplifies the graph to remove duplicate edges and loops

4.  Calculates Euclidean distances between connected vertices

5.  Adds vertex coordinates and edge distances as attributes to the
    graph

Note that the input `nodes` matrix should use 0-based indexing (starting
from 0), as the function will increment indices by 1 when creating the
graph.

## See also

[`SurfaceGeometry`](SurfaceGeometry.md),
[`graph_from_edgelist`](https://r.igraph.org/reference/graph_from_edgelist.html)

## Examples

``` r
# \donttest{
# Create a simple cube mesh with 8 vertices
vertices <- matrix(c(
  0, 0, 0,  # vertex 1
  1, 0, 0,  # vertex 2
  1, 1, 0,  # vertex 3
  0, 1, 0,  # vertex 4
  0, 0, 1,  # vertex 5
  1, 0, 1,  # vertex 6
  1, 1, 1,  # vertex 7
  0, 1, 1   # vertex 8
), ncol = 3, byrow = TRUE)

# Define triangular faces with 0-based indices
faces <- matrix(c(
  # bottom face (z=0)
  0, 1, 2,
  0, 2, 3,
  # top face (z=1)
  4, 5, 6,
  4, 6, 7,
  # front face (y=0)
  0, 1, 5,
  0, 5, 4,
  # back face (y=1)
  2, 3, 7,
  2, 7, 6,
  # left face (x=0)
  0, 3, 7,
  0, 7, 4,
  # right face (x=1)
  1, 2, 6,
  1, 6, 5
), ncol = 3, byrow = TRUE)

# Create the graph representation of the mesh
graph <- meshToGraph(vertices, faces)

# Examine the graph properties
cat("Number of vertices:", igraph::vcount(graph), "\n")
#> Number of vertices: 8 
cat("Number of edges:", igraph::ecount(graph), "\n")
#> Number of edges: 18 

# Plot the graph if igraph is available
if (requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("rgl", quietly = TRUE)) {
  # First visualize the mesh
  rgl::open3d()
  mesh <- rgl::tmesh3d(
    vertices = t(vertices),
    indices = t(faces) + 1,  # rgl uses 1-based indexing
    homogeneous = FALSE
  )
  rgl::shade3d(mesh, col = "lightblue")
  rgl::title3d(main = "Original Mesh")

  # Visualize the graph connections using the 3D coordinates
  rgl::open3d()
  # Plot vertices
  rgl::points3d(vertices[,1], vertices[,2], vertices[,3], size = 10, col = "red")
  # Plot edges
  edges <- igraph::as_edgelist(graph)
  for (i in 1:nrow(edges)) {
    v1 <- edges[i, 1]
    v2 <- edges[i, 2]
    coords <- vertices[c(v1, v2), ]
    rgl::lines3d(coords[,1], coords[,2], coords[,3], col = "black", lwd = 2)
  }
  rgl::title3d(main = "Graph Representation")
}
3D plot

{"x":{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":true,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmode":"modulate","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false,"polygon_offset":[0,0],"margin":"","floating":false,"tag":"","blend":["src_alpha","one_minus_src_alpha"]},"rootSubscene":144,"objects":{"151":{"id":151,"type":"points","material":{"lit":false,"size":10},"vertices":"0","colors":"1","centers":"2","ignoreExtent":false,"flags":34816},"152":{"id":152,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"3","colors":"4","centers":"5","ignoreExtent":false,"flags":41024},"153":{"id":153,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"6","colors":"7","centers":"8","ignoreExtent":false,"flags":41024},"154":{"id":154,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"9","colors":"10","centers":"11","ignoreExtent":false,"flags":41024},"155":{"id":155,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"12","colors":"13","centers":"14","ignoreExtent":false,"flags":41024},"156":{"id":156,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"15","colors":"16","centers":"17","ignoreExtent":false,"flags":41024},"157":{"id":157,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"18","colors":"19","centers":"20","ignoreExtent":false,"flags":41024},"158":{"id":158,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"21","colors":"22","centers":"23","ignoreExtent":false,"flags":41024},"159":{"id":159,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"24","colors":"25","centers":"26","ignoreExtent":false,"flags":41024},"160":{"id":160,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"27","colors":"28","centers":"29","ignoreExtent":false,"flags":41024},"161":{"id":161,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"30","colors":"31","centers":"32","ignoreExtent":false,"flags":41024},"162":{"id":162,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"33","colors":"34","centers":"35","ignoreExtent":false,"flags":41024},"163":{"id":163,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"36","colors":"37","centers":"38","ignoreExtent":false,"flags":41024},"164":{"id":164,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"39","colors":"40","centers":"41","ignoreExtent":false,"flags":41024},"165":{"id":165,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"42","colors":"43","centers":"44","ignoreExtent":false,"flags":41024},"166":{"id":166,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"45","colors":"46","centers":"47","ignoreExtent":false,"flags":41024},"167":{"id":167,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"48","colors":"49","centers":"50","ignoreExtent":false,"flags":41024},"168":{"id":168,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"51","colors":"52","centers":"53","ignoreExtent":false,"flags":41024},"169":{"id":169,"type":"linestrip","material":{"lit":false,"lwd":2},"vertices":"54","colors":"55","centers":"56","ignoreExtent":false,"flags":41024},"171":{"id":171,"type":"text","material":{"lit":false,"margin":0,"edge":[0,1,1]},"vertices":"57","colors":"58","texts":[["Graph Representation"]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"59","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"148":{"id":148,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"150":{"id":150,"type":"background","material":{"lit":false,"back":"lines"},"colors":"60","centers":"61","sphere":false,"fogtype":"none","fogscale":1,"flags":32768},"170":{"id":170,"type":"bboxdeco","material":{"front":"culled","back":"culled"},"colors":"62","axes":{"mode":["none","none","none"],"step":[-1,-1,-1],"nticks":[0,0,0],"marklen":[15,15,15],"expand":[1.029999971389771,1.029999971389771,1.029999971389771]},"draw_front":false,"flags":32769},"144":{"id":144,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":144,"mouseMode":{"none":"none","left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,4.238348960876465],"modelMatrix":[[1,0,0,-0.5],[0,0.3420201539993286,0.9396926164627075,-0.6408563852310181],[0,-0.9396926164627075,0.3420201539993286,-3.939512729644775],[0,0,0,1]],"projMatrix":[[3.732050895690918,0,0,0],[0,3.732050895690918,0,0],[0,0,-3.863702774047852,-15.27875804901123],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201433256682,0.9396926207859085,0],[0,-0.9396926207859085,0.3420201433256682,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[0,1,0,1,0,1],"windowRect":[0,0,256,256],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"NULL","maxClipPlanes":2147483647,"glVersion":"NA","activeSubscene":0},"embeddings":{"viewport":"replace","projection":"replace","model":"replace","mouse":"replace"},"objects":[150,170,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,148],"subscenes":[],"flags":44369}},"crosstalk":{"key":[],"group":[],"id":[],"options":[]},"width":700,"height":432.6328800988875,"buffer":{"accessors":[{"bufferView":0,"componentType":5121,"count":8,"type":"VEC3"},{"bufferView":1,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":2,"componentType":5121,"count":8,"type":"VEC3"},{"bufferView":3,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":4,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":5,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":6,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":7,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":8,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":9,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":10,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":11,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":12,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":13,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":14,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":15,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":16,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":17,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":18,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":19,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":20,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":21,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":22,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":23,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":24,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":25,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":26,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":27,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":28,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":29,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":30,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":31,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":32,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":33,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":34,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":35,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":36,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":37,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":38,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":39,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":40,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":41,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":42,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":43,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":44,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":45,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":46,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":47,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":48,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":49,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":50,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":51,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":52,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":53,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":54,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":55,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":56,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":57,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":58,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":59,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":60,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":61,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":62,"componentType":5121,"count":1,"type":"VEC4"}],"bufferViews":[{"buffer":0,"byteLength":24,"byteOffset":0},{"buffer":0,"byteLength":4,"byteOffset":24},{"buffer":0,"byteLength":24,"byteOffset":28},{"buffer":0,"byteLength":6,"byteOffset":52},{"buffer":0,"byteLength":4,"byteOffset":58},{"buffer":0,"byteLength":6,"byteOffset":62},{"buffer":0,"byteLength":6,"byteOffset":68},{"buffer":0,"byteLength":4,"byteOffset":74},{"buffer":0,"byteLength":6,"byteOffset":78},{"buffer":0,"byteLength":6,"byteOffset":84},{"buffer":0,"byteLength":4,"byteOffset":90},{"buffer":0,"byteLength":6,"byteOffset":94},{"buffer":0,"byteLength":6,"byteOffset":100},{"buffer":0,"byteLength":4,"byteOffset":106},{"buffer":0,"byteLength":6,"byteOffset":110},{"buffer":0,"byteLength":6,"byteOffset":116},{"buffer":0,"byteLength":4,"byteOffset":122},{"buffer":0,"byteLength":6,"byteOffset":126},{"buffer":0,"byteLength":6,"byteOffset":132},{"buffer":0,"byteLength":4,"byteOffset":138},{"buffer":0,"byteLength":6,"byteOffset":142},{"buffer":0,"byteLength":6,"byteOffset":148},{"buffer":0,"byteLength":4,"byteOffset":154},{"buffer":0,"byteLength":6,"byteOffset":158},{"buffer":0,"byteLength":6,"byteOffset":164},{"buffer":0,"byteLength":4,"byteOffset":170},{"buffer":0,"byteLength":6,"byteOffset":174},{"buffer":0,"byteLength":6,"byteOffset":180},{"buffer":0,"byteLength":4,"byteOffset":186},{"buffer":0,"byteLength":6,"byteOffset":190},{"buffer":0,"byteLength":6,"byteOffset":196},{"buffer":0,"byteLength":4,"byteOffset":202},{"buffer":0,"byteLength":6,"byteOffset":206},{"buffer":0,"byteLength":6,"byteOffset":212},{"buffer":0,"byteLength":4,"byteOffset":218},{"buffer":0,"byteLength":6,"byteOffset":222},{"buffer":0,"byteLength":6,"byteOffset":228},{"buffer":0,"byteLength":4,"byteOffset":234},{"buffer":0,"byteLength":6,"byteOffset":238},{"buffer":0,"byteLength":6,"byteOffset":244},{"buffer":0,"byteLength":4,"byteOffset":250},{"buffer":0,"byteLength":6,"byteOffset":254},{"buffer":0,"byteLength":6,"byteOffset":260},{"buffer":0,"byteLength":4,"byteOffset":266},{"buffer":0,"byteLength":6,"byteOffset":270},{"buffer":0,"byteLength":6,"byteOffset":276},{"buffer":0,"byteLength":4,"byteOffset":282},{"buffer":0,"byteLength":6,"byteOffset":286},{"buffer":0,"byteLength":6,"byteOffset":292},{"buffer":0,"byteLength":4,"byteOffset":298},{"buffer":0,"byteLength":6,"byteOffset":302},{"buffer":0,"byteLength":6,"byteOffset":308},{"buffer":0,"byteLength":4,"byteOffset":314},{"buffer":0,"byteLength":6,"byteOffset":318},{"buffer":0,"byteLength":6,"byteOffset":324},{"buffer":0,"byteLength":4,"byteOffset":330},{"buffer":0,"byteLength":6,"byteOffset":334},{"buffer":0,"byteLength":12,"byteOffset":340},{"buffer":0,"byteLength":4,"byteOffset":352},{"buffer":0,"byteLength":12,"byteOffset":356},{"buffer":0,"byteLength":4,"byteOffset":368},{"buffer":0,"byteLength":3,"byteOffset":372},{"buffer":0,"byteLength":4,"byteOffset":375}],"buffers":[{"byteLength":379,"bytes":"AAAAAQAAAQEAAAEAAAABAQABAQEBAAEBAQAAAQAAAAEAAAEBAAABAAAAAQEAAQEBAQABAQAA\nAAEAAAAAAAEAAAABAAAAAAABAQAAAAABAAAAAQEAAAAAAAEAAAAAAQAAAAABAAAAAAAAAQAA\nAAEAAAAAAAEAAAABAAEAAAABAAAAAQABAAAAAAEBAAAAAQAAAAABAQEAAAEBAAAAAAEBAAAB\nAQABAAABAAEAAAABAQAAAQABAQAAAQEBAAAAAQEAAAEBAQEBAAABAAAAAAEBAQAAAQABAQAB\nAQEAAAABAQEAAQEBAQEAAAEBAAAAAQEBAAABAQABAAABAQAAAAEAAQAAAQEAAAEBAAEAAAAB\nAAABAQABAAABAQEBAAAAAQAAAQEBAQAAAQABAQAAAAEAAAEAAQEBAAEBAQEAAAABAQABAQEB\nAQEBAAEBAAAAAQEBAQABAQAAwH8AAABAAAAAQAAAAAEAAMB/AAAAQAAAAEABAQEBAAAAAAAA\nAQ=="}]},"context":{"shiny":false,"rmarkdown":null},"vertexShader":"#line 2 1\n// File 1 is the vertex shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\n\nattribute vec3 aPos;\nattribute vec4 aCol;\nuniform mat4 mvMatrix;\nuniform mat4 prMatrix;\nvarying vec4 vCol;\nvarying vec4 vPosition;\n\n#ifdef NEEDS_VNORMAL\nattribute vec3 aNorm;\nuniform mat4 normMatrix;\nvarying vec4 vNormal;\n#endif\n\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nattribute vec2 aTexcoord;\nvarying vec2 vTexcoord;\n#endif\n\n#ifdef FIXED_SIZE\nuniform vec3 textScale;\n#endif\n\n#ifdef FIXED_QUADS\nattribute vec3 aOfs;\n#endif\n\n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\nvarying float normz;\nuniform mat4 invPrMatrix;\n#else\nattribute vec3 aPos1;\nattribute vec3 aPos2;\nvarying float normz;\n#endif\n#endif // IS_TWOSIDED\n\n#ifdef FAT_LINES\nattribute vec3 aNext;\nattribute vec2 aPoint;\nvarying vec2 vPoint;\nvarying float vLength;\nuniform float uAspect;\nuniform float uLwd;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  \n#ifndef IS_BRUSH\n#if defined(NCLIPPLANES) || !defined(FIXED_QUADS) || defined(HAS_FOG) || defined(USE_ENVMAP)\n  vPosition = mvMatrix * vec4(aPos, 1.);\n#endif\n  \n#ifndef FIXED_QUADS\n  gl_Position = prMatrix * vPosition;\n#endif\n#endif // !IS_BRUSH\n  \n#ifdef IS_POINTS\n  gl_PointSize = POINTSIZE;\n#endif\n  \n  vCol = aCol;\n  \n// USE_ENVMAP implies NEEDS_VNORMAL\n\n#ifdef NEEDS_VNORMAL\n  vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));\n#endif\n\n#ifdef USE_ENVMAP\n  vReflection = normalize(reflect(vPosition.xyz/vPosition.w, \n                        normalize(vNormal.xyz/vNormal.w)));\n#endif\n  \n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\n  /* normz should be calculated *after* projection */\n  normz = (invPrMatrix*vNormal).z;\n#else\n  vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n  pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n  vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n  pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n  normz = pos1.x*pos2.y - pos1.y*pos2.x;\n#endif\n#endif // IS_TWOSIDED\n  \n#ifdef NEEDS_VNORMAL\n  vNormal = vec4(normalize(vNormal.xyz), 1);\n#endif\n  \n#if defined(HAS_TEXTURE) || defined(IS_TEXT)\n  vTexcoord = aTexcoord;\n#endif\n  \n#if defined(FIXED_SIZE) && !defined(ROTATING)\n  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w;\n  gl_Position = pos + vec4(aOfs*textScale, 0.);\n#endif\n  \n#if defined(IS_SPRITES) && !defined(FIXED_SIZE)\n  vec4 pos = mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w + vec4(aOfs,  0.);\n  gl_Position = prMatrix*pos;\n#endif\n  \n#ifdef FAT_LINES\n  /* This code was inspired by Matt Deslauriers' code in \n   https://mattdesl.svbtle.com/drawing-lines-is-hard */\n  vec2 aspectVec = vec2(uAspect, 1.0);\n  mat4 projViewModel = prMatrix * mvMatrix;\n  vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n  currentProjected = currentProjected/currentProjected.w;\n  vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n  vec2 currentScreen = currentProjected.xy * aspectVec;\n  vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n  float len = uLwd;\n  vec2 dir = vec2(1.0, 0.0);\n  vPoint = aPoint;\n  vLength = length(nextScreen - currentScreen)/2.0;\n  vLength = vLength/(vLength + len);\n  if (vLength > 0.0) {\n    dir = normalize(nextScreen - currentScreen);\n  }\n  vec2 normal = vec2(-dir.y, dir.x);\n  dir.x /= uAspect;\n  normal.x /= uAspect;\n  vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n  gl_Position = currentProjected + offset;\n#endif\n  \n#ifdef IS_BRUSH\n  gl_Position = vec4(aPos, 1.);\n#endif\n}","fragmentShader":"#line 2 2\n// File 2 is the fragment shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\nvarying vec4 vCol; // carries alpha\nvarying vec4 vPosition;\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nvarying vec2 vTexcoord;\nuniform sampler2D uSampler;\n#endif\n\n#ifdef HAS_FOG\nuniform int uFogMode;\nuniform vec3 uFogColor;\nuniform vec4 uFogParms;\n#endif\n\n#if defined(IS_LIT) && !defined(FIXED_QUADS)\nvarying vec4 vNormal;\n#endif\n\n#if NCLIPPLANES > 0\nuniform vec4 vClipplane[NCLIPPLANES];\n#endif\n\n#if NLIGHTS > 0\nuniform mat4 mvMatrix;\n#endif\n\n#ifdef IS_LIT\nuniform vec3 emission;\nuniform float shininess;\n#if NLIGHTS > 0\nuniform vec3 ambient[NLIGHTS];\nuniform vec3 specular[NLIGHTS]; // light*material\nuniform vec3 diffuse[NLIGHTS];\nuniform vec3 lightDir[NLIGHTS];\nuniform bool viewpoint[NLIGHTS];\nuniform bool finite[NLIGHTS];\n#endif\n#endif // IS_LIT\n\n#ifdef IS_TWOSIDED\nuniform bool front;\nvarying float normz;\n#endif\n\n#ifdef FAT_LINES\nvarying vec2 vPoint;\nvarying float vLength;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  vec4 fragColor;\n#ifdef FAT_LINES\n  vec2 point = vPoint;\n  bool neg = point.y < 0.0;\n  point.y = neg ? (point.y + vLength)/(1.0 - vLength) :\n                 -(point.y - vLength)/(1.0 - vLength);\n#if defined(IS_TRANSPARENT) && defined(IS_LINESTRIP)\n  if (neg && length(point) <= 1.0) discard;\n#endif\n  point.y = min(point.y, 0.0);\n  if (length(point) > 1.0) discard;\n#endif // FAT_LINES\n  \n#ifdef ROUND_POINTS\n  vec2 coord = gl_PointCoord - vec2(0.5);\n  if (length(coord) > 0.5) discard;\n#endif\n  \n#if NCLIPPLANES > 0\n  for (int i = 0; i < NCLIPPLANES; i++)\n    if (dot(vPosition, vClipplane[i]) < 0.0) discard;\n#endif\n    \n#ifdef FIXED_QUADS\n    vec3 n = vec3(0., 0., 1.);\n#elif defined(IS_LIT)\n    vec3 n = normalize(vNormal.xyz);\n#endif\n    \n#ifdef IS_TWOSIDED\n    if ((normz <= 0.) != front) discard;\n#endif\n\n#ifdef IS_LIT\n    vec3 eye = normalize(-vPosition.xyz/vPosition.w);\n    vec3 lightdir;\n    vec4 colDiff;\n    vec3 halfVec;\n    vec4 lighteffect = vec4(emission, 0.);\n    vec3 col;\n    float nDotL;\n#ifdef FIXED_QUADS\n    n = -faceforward(n, n, eye);\n#endif\n    \n#if NLIGHTS > 0\n    // Simulate two-sided lighting\n    if (n.z < 0.0)\n      n = -n;\n    for (int i=0;i<NLIGHTS;i++) {\n      colDiff = vec4(vCol.rgb * diffuse[i], vCol.a);\n      lightdir = lightDir[i];\n      if (!viewpoint[i]) {\n        if (finite[i]) {\n          lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n        } else {\n          lightdir = (mvMatrix * vec4(lightdir, 0.)).xyz;\n        }\n      }\n      if (!finite[i]) {\n        halfVec = normalize(lightdir + eye);\n      } else {\n        lightdir = normalize(lightdir - vPosition.xyz/vPosition.w);\n        halfVec = normalize(lightdir + eye);\n      }\n      col = ambient[i];\n      nDotL = dot(n, lightdir);\n      col = col + max(nDotL, 0.) * colDiff.rgb;\n      col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular[i];\n      lighteffect = lighteffect + vec4(col, colDiff.a);\n    }\n#endif\n    \n#else // not IS_LIT\n    vec4 colDiff = vCol;\n    vec4 lighteffect = colDiff;\n#endif\n    \n#ifdef IS_TEXT\n    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n#endif\n    \n#ifdef HAS_TEXTURE\n\n// These calculations use the definitions from \n// https://docs.gl/gl3/glTexEnv\n\n#ifdef USE_ENVMAP\n    float m = 2.0 * sqrt(dot(vReflection, vReflection) + 2.0*vReflection.z + 1.0);\n    vec4 textureColor = texture2D(uSampler, vReflection.xy / m + vec2(0.5, 0.5));\n#else\n    vec4 textureColor = texture2D(uSampler, vTexcoord);\n#endif\n\n#ifdef TEXTURE_rgb\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(textureColor.rgb, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*vec4(textureColor.rgb, 1.);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb, lighteffect.a);\n#endif\n\n#endif //TEXTURE_rgb\n        \n#ifdef TEXTURE_rgba\n\n#ifdef TEXMODE_replace\n// already done\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*textureColor;\n#endif\n\n#ifdef TEXMODE_decal\n    textureColor = vec4((1. - textureColor.a)*lighteffect.rgb) +\n                     textureColor.a*textureColor.rgb, \n                     lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n    \n#endif //TEXTURE_rgba\n    \n#ifdef TEXTURE_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(lighteffect.rgb, luminance);\n#endif \n\n#if defined(TEXMODE_modulate) || defined(TEXMODE_blend) || defined(TEXMODE_add)\n    textureColor = vec4(lighteffect.rgb, lighteffect.a*luminance);\n#endif\n \n#endif // TEXTURE_alpha\n    \n// The TEXTURE_luminance values are not from that reference    \n#ifdef TEXTURE_luminance\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, lighteffect.a);\n#endif\n\n#endif // TEXTURE_luminance\n \n    \n#ifdef TEXTURE_luminance_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, textureColor.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n\n#endif\n\n#endif // TEXTURE_luminance_alpha\n    \n    fragColor = textureColor;\n\n#elif defined(IS_TEXT)\n    if (textureColor.a < 0.1)\n      discard;\n    else\n      fragColor = textureColor;\n#else\n    fragColor = lighteffect;\n#endif // HAS_TEXTURE\n    \n#ifdef HAS_FOG\n    // uFogParms elements: x = near, y = far, z = fogscale, w = (1-sin(FOV/2))/(1+sin(FOV/2))\n    // In Exp and Exp2: use density = density/far\n    // fogF will be the proportion of fog\n    // Initialize it to the linear value\n    float fogF;\n    if (uFogMode > 0) {\n      fogF = (uFogParms.y - vPosition.z/vPosition.w)/(uFogParms.y - uFogParms.x);\n      if (uFogMode > 1)\n        fogF = mix(uFogParms.w, 1.0, fogF);\n      fogF = fogF*uFogParms.z;\n      if (uFogMode == 2)\n        fogF = 1.0 - exp(-fogF);\n      // Docs are wrong: use (density*c)^2, not density*c^2\n      // https://gitlab.freedesktop.org/mesa/mesa/-/blob/master/src/mesa/swrast/s_fog.c#L58\n      else if (uFogMode == 3)\n        fogF = 1.0 - exp(-fogF*fogF);\n      fogF = clamp(fogF, 0.0, 1.0);\n      gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);\n    } else gl_FragColor = fragColor;\n#else\n    gl_FragColor = fragColor;\n#endif // HAS_FOG\n    \n}","players":[],"webGLoptions":{"preserveDrawingBuffer":true},"fastTransparency":true},"evals":[],"jsHooks":[]}# }
```
