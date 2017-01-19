#' Generic function to construct neighborhood graph from surface mesh using edge weights.
#' @param x surface mesh
#' @param radius the edge radius defining the neighborhood
#' @param edgeWeights vector of edgeWeights used to define edge distance.
#' @param nodes the subset of nodes to use
#' @param ... extra arguments
#' @exportMethod neighborGraph
#' @rdname neighborGraph-methods
setGeneric(name="neighborGraph", def=function(x, radius, edgeWeights, nodes, ...) standardGeneric("neighborGraph"))

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


#' get compute graph laplacian
#' @param x the object to get Laplacian from
#' @param normalized logical indicating whether laplcian is normalized
#' @param weights edge weights for weighted Laplacian matrix
#' @param ... extra args
#' @rdname graph-methods
setGeneric(name="laplacian", def=function(x, normalized, weights, ...) standardGeneric("laplacian"))


#' get adjacency graph
#' @param x graph structure
#' @param weights a \code{character} string indicating the edge attribute in the \code{igraph} object for the weights. If absent, weights are 0, 1.
#' @param ... extra args
#' @rdname graph-methods
setGeneric(name="adjacency", def=function(x, attr, ...) standardGeneric("adjacency"))

#' get left hemisphere
#' @param x the surface
setGeneric(name="left", def=function(x) standardGeneric("left"))

#' get right hemisphere
#' @param x the surface
setGeneric(name="right", def=function(x) standardGeneric("right"))





