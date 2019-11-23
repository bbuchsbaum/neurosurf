#' @importMethodsFrom neuroim2 data_reader
NULL

#' @export
setGeneric("plot", function(x, ...) standardGeneric("plot"))



#' function to construct neighborhood graph from surface mesh using edge weights.
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
#'
#' @param x the surface object
#' @param ... extra args
#' @rdname vertices-methods
setGeneric(name="vertices", def=function(x,...) standardGeneric("vertices"))

#' extract surface node numbers
#'
#' @param x the surface object
#' @rdname nodes-methods
setGeneric(name="nodes", def=function(x) standardGeneric("nodes"))

#' extract geometry object
#'
#' @param x the surface object
#' @rdname geometry-methods
setGeneric(name="geometry", def=function(x) standardGeneric("geometry"))


#' extract \code{igraph} object
#' @param x the object to extract the graph from
#' @param ... extra args
#' @rdname graph-methods
setGeneric(name="graph", def=function(x, ...) standardGeneric("graph"))

#' smooth a surface
#'
#' @param x the surface object to smooth
#' @param ... extra args
#' @rdname smooth-methods
setGeneric(name="smooth", def=function(x, ...) standardGeneric("smooth"))


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

#' cluster_threshold
#'
#' @param x the object to threshold
#' @param threshold the numeric threshold range
#' @param size the minimum cluster size
#' @param ... extra args
#' @seealso conn_comp
#' @export
setGeneric(name="cluster_threshold", def=function(x, threshold, size, ...) standardGeneric("cluster_threshold"))


# Generic function to read image meta info given a file and a \code{\linkS4class{FileFormat}} instance.
# @param x file format
# @param file_name file name contianing meta information
# @export
# @rdname read_meta_info-methods
#setGeneric(name="read_meta_info", def=function(x, file_name) standardGeneric("read_meta_info"))





