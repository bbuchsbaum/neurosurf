#' Create an instance of class \code{\linkS4class{ROISurface}}
#'
#' @param geometry the parent geometry: an instance of class \code{SurfaceGeometry}
#' @param indices the parent surface indices
#' @param data the data values, numeric \code{vector}
#' @return an instance of class \code{ROISurface}
#' @rdname ROISurface
#' @export
ROISurface <- function(geometry, indices, data) {
  vert <- vertices(geometry, indices)
  new("ROISurface", geometry=geometry, data=data, coords=vert, indices=indices)
}


#' Create an instance of class \code{\linkS4class{ROISurfaceVector}}
#'
#' @param geometry the parent geometry: an instance of class \code{SurfaceGeometry}
#' @param indices the parent surface indices
#' @param data the data values, a \code{matrix}
#' @return an instance of class \code{ROISurfaceVector}
#' @rdname ROISurfaceVector
#' @export
ROISurfaceVector <- function(geometry, indices, data) {
  vert <- vertices(geometry, indices)
  new("ROISurfaceVector", geometry=geometry, data=data, coords=vert, indices=indices)
}

#' convert a \code{ROISurfaceVector} to an augmented matrix
#'
#' @rdname as.matrix-methods
#' @param x the object
#' @export
setMethod(f="as.matrix", signature=signature(x = "ROISurfaceVector"), def=function(x) {
  as(x, "matrix")
})


#' @title Create a Region on Surface
#'
#' @description Creates a Region on a Surface from a radius and surface
#'
#' @param surf a \code{SurfaceGeometry} or \code{BrainSurface} or \code{BrainSurfaceVector}
#' @param index the index of the central surface node
#' @param radius the size in mm of the geodesic radius
#' @param max_order maximum number of edges to traverse.
#'   default is computed based on average edge length.
#' @importFrom assertthat assert_that
#' @importFrom igraph E V ego distances induced_subgraph V neighborhood
#' @rdname SurfaceDisk
#' @export
SurfaceDisk <- function(surf, index, radius, max_order=NULL) {
  assertthat::assert_that(length(index) == 1)

  edgeWeights=igraph::E(surf@graph)$dist

  if (is.null(max_order)) {
    avg_weight <- mean(edgeWeights)
    max_order <- ceiling(radius/avg_weight) + 1
  }

  cand <- as.vector(igraph::ego(surf@graph, order= max_order, nodes=index)[[1]])
  D <- igraph::distances(surf@graph, index, cand, weights=edgeWeights, algorithm="dijkstra")
  keep <- which(D < radius)

  if (inherits(surf, "BrainSurface") || inherits(surf, "BrainSurfaceVector")) {
    ROISurfaceVector(surf@geometry, indices=cand[keep], data=as.matrix(series(surf, keep)))
  } else if (inherits(surf, "SurfaceGeometry")) {
    ROISurface(surf, indices=cand[keep], rep(1, length(keep)))
  }

}


roi_surface_matrix <- function(mat, refspace, indices, coords) {
  structure(mat,
            refspace=refspace,
            indices=indices,
            coords=coords,
            class=c("roi_surface_matrix", "matrix"))

}

#' values
#'
#' @rdname values-methods
#' @importMethodsFrom neuroim2 values
#' @export
setMethod("values", signature(x="ROISurface"),
          function(x, ...) {
            x@data
          })


#' values
#'
#' @rdname values-methods
#' @export
setMethod("values", signature(x="ROISurfaceVector"),
          function(x, ...) {
            x@data
          })

#' indices
#'
#' @rdname indices-methods
#' @export
setMethod("indices", signature(x="ROISurface"),
          function(x) {
            x@indices
          })

#' indices
#'
#' @rdname indices-methods
#' @export
setMethod("indices", signature(x="ROISurfaceVector"),
          function(x) {
            x@indices
          })

#' indices
#' @export
#' @rdname coords-methods
setMethod(f="coords", signature=signature(x="ROISurface"),
          function(x) {
            x@coords
          })


#' indices
#' @export
#' @rdname length-methods
#' @param x the object to get \code{length}
setMethod(f="length", signature=signature(x="ROISurface"),
          function(x) {
            length(x@indices)
          })


#' subset an \code{ROISurface}
#'
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param drop drop dimension
#' @rdname surf_subset-methods
#' @aliases [,ROISurface,numeric,missing,ANY-method
setMethod("[", signature=signature(x = "ROISurface", i = "numeric", j = "missing", drop = "ANY"),
          function (x, i, j, drop) {
            ROISurface(x@geometry, x@indices[i], x@data[i])
          })


#' show an \code{\linkS4class{ROISurface}}
#' @param object the object
#' @export
setMethod("show", signature=signature(object = "ROISurface"),
          function (object) {
            cat("\n\n\tROISurface", "\n")
            cat("\tsize: ", length(object@indices), "\n")
            cat("\tdata type:", if (is.matrix(object@data)) "matrix" else "vector", "\n" )
            cat("\tdata dim:", if (is.matrix(object@data)) dim(object@data) else length(object@data), "\n" )
            cat("\tvertex center of mass: ", colMeans(object@coords), "\n")
          })
