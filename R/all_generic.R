#' @importMethodsFrom neuroim2 data_reader
NULL


#' Generic function to construct neighborhood graph from surface mesh using edge weights.
#'
#' @param x surface mesh
#' @param radius the edge radius defining the neighborhood
#' @param edgeWeights vector of edgeWeights used to define edge distance.
#' @param nodes the subset of nodes to use
#' @param ... extra arguments
#' @exportMethod neighbor_graph
#' @rdname neighbor_graph-methods
setGeneric(name="neighbor_graph", def=function(x, radius, edgeWeights, nodes, ...) standardGeneric("neighbor_graph"))

#' extract vertices from surface
#' @param x the surface object
#' @param ... extra args
#' @rdname vertices-methods
setGeneric(name="vertices", def=function(x,...) standardGeneric("vertices"))

#' extract surface node numbers
#' @param x the surface object
#' @rdname nodes-methods
setGeneric(name="nodes", def=function(x) standardGeneric("nodes"))

#' extract geometry object
#' @param x the surface object
#' @rdname geometry-methods
setGeneric(name="geometry", def=function(x) standardGeneric("geometry"))


#' extract \code{igraph} object
#' @param x the object to extract the graph from
#' @param ... extra args
#' @rdname graph-methods
setGeneric(name="graph", def=function(x, ...) standardGeneric("graph"))

#' smooth a surface
#' @param x the surface object to smooth
#' @param sigma the bandwidth
#' @param ... extra args
#' @rdname smooth-methods
setGeneric(name="smooth", def=function(x, sigma, ...) standardGeneric("smooth"))


#' laplacian
#'
#' get compute graph laplacian
#' @param x the object to get Laplacian from
#' @param normalized logical indicating whether laplcian is normalized
#' @param weights edge weights for weighted Laplacian matrix
#' @param ... extra args
#' @rdname graph-methods
setGeneric(name="laplacian", def=function(x, normalized, weights, ...) standardGeneric("laplacian"))

#' adjacency
#'
#' get adjacency graph
#' @param x graph structure
#' @param weights a \code{character} string indicating the edge attribute in the \code{igraph} object for the weights. If absent, weights are 0, 1.
#' @param ... extra args
#' @rdname graph-methods
#' @rdname adjacency-methods
setGeneric(name="adjacency", def=function(x, attr, ...) standardGeneric("adjacency"))

#' blend_colors
#'
#' blend two color planes
#' @param bottom the bottom color plane
#' @param top the top color plane
#' @param alpha the alpha blending value
setGeneric(name="blend_colors", def=function(bottom, top, alpha) standardGeneric("blend_colors"))

#' color mapping
#'
#' map data values to a set of colors
#'
#' @param x the object to map over
setGeneric(name="map_colors", def=function(x, ...) standardGeneric("map_colors"))

#' color conversion
#'
#' convert to rgb colors
#'
#' @param x the object to convert
#' @param ... extra args
#' @rdname color-conversion
setGeneric(name="as_rgb", def=function(x, ...) standardGeneric("as_rgb"))


#' @rdname color-conversion
setGeneric(name="as_hexcol", def=function(x, ...) standardGeneric("as_hexcol"))

#' alpha_channel
#'
#' extract the alpha channel
#'
#' @param x the object to extract alpha channel from
#' @param ... extra args
setGeneric(name="alpha_channel", def=function(x, ...) standardGeneric("alpha_channel"))


#' curvature
#'
#' @param x the object to get curvature from
#' @param ... extra args
#' @export
setGeneric(name="curvature", def=function(x, ...) standardGeneric("curvature"))

#' left
#'
#' get left hemisphere
#' @param x the surface
setGeneric(name="left", def=function(x) standardGeneric("left"))


#' right
#'
#' get right hemisphere
#' @param x the surface
setGeneric(name="right", def=function(x) standardGeneric("right"))



#' Generic function to read image meta info given a file and a \code{\linkS4class{FileFormat}} instance.
#' @param x file format
#' @param file_name file name contianing meta information
#' @export read_meta_info
#' @rdname read_meta_info-methods
setGeneric(name="read_meta_info", def=function(x, file_name) standardGeneric("read_meta_info"))





