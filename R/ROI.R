#' Create an instance of class \code{\linkS4class{ROISurface}}
#'
#' @param geometry the parent geometry: an instance of class \code{SurfaceGeometry}
#' @param indices the parent surface indices
#' @param data the data values, numeric \code{vector}
#' @return an instance of class \code{ROISurface}
#' @examples
#' \donttest{
#' verts <- matrix(c(0,0,0,
#'                   1,0,0,
#'                   0,1,0), ncol=3, byrow=TRUE)
#' faces <- matrix(c(1,2,3), ncol=3, byrow=TRUE)
#' geom <- SurfaceGeometry(verts, faces, "lh")
#'
#' ROISurface(geom, 1L, 1)
#'
#' try(ROISurface(geom, 4L, 1))      # out of range
#' try(ROISurface(geom, 1.5, 1))     # non-integer
#' }
#' @rdname ROISurface
#' @export
ROISurface <- function(geometry, indices, data) {
  all_nodes <- nodes(geometry)

  if (!is.integer(indices)) {
    if (is.numeric(indices) && all(indices == as.integer(indices))) {
      indices <- as.integer(indices)
    } else {
      stop("'indices' must be integer")
    }
  }

  if (any(indices < 1L) || any(indices > length(all_nodes))) {
    stop("'indices' are out of bounds for provided 'geometry'")
  }

  vert <- vertices(geometry, indices)
  new("ROISurface", geometry=geometry, data=data, coords=vert, indices=indices)
}


#' Create an instance of class \code{\linkS4class{ROISurfaceVector}}
#'
#' @param geometry the parent geometry: an instance of class \code{SurfaceGeometry}
#' @param indices the parent surface indices
#' @param data the data values, a \code{matrix}
#' @return an instance of class \code{ROISurfaceVector}
#' @examples
#' \donttest{
#' verts <- matrix(c(0,0,0,
#'                   1,0,0,
#'                   0,1,0), ncol=3, byrow=TRUE)
#' faces <- matrix(c(1,2,3), ncol=3, byrow=TRUE)
#' geom <- SurfaceGeometry(verts, faces, "lh")
#'
#' vec <- matrix(c(0.5, 1.5), nrow=1)
#' ROISurfaceVector(geom, c(1L,2L), vec)
#'
#' try(ROISurfaceVector(geom, c(1L,4L), vec))   # out of range
#' try(ROISurfaceVector(geom, c(1,2.5), vec))   # non-integer
#' }
#' @rdname ROISurfaceVector
#' @export
ROISurfaceVector <- function(geometry, indices, data) {
  all_nodes <- nodes(geometry)

  if (!is.integer(indices)) {
    if (is.numeric(indices) && all(indices == as.integer(indices))) {
      indices <- as.integer(indices)
    } else {
      stop("'indices' must be integer")
    }
  }

  if (any(indices < 1L) || any(indices > length(all_nodes))) {
    stop("'indices' are out of bounds for provided 'geometry'")
  }

  vert <- vertices(geometry, indices)
  new("ROISurfaceVector", geometry=geometry, data=data, coords=vert, indices=indices)
}

#' convert a \code{ROISurfaceVector} to an augmented matrix
#'
#' @rdname as.matrix
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
#' @param index the index of the central surface node. Must be a numeric
#'   integer value within \code{1:length(V(surf@graph))}.
#' @param radius the size in mm of the geodesic radius. Must be a single
#'   positive numeric value.
#' @param max_order maximum number of edges to traverse.
#'   default is computed based on average edge length.
#' @details The igraph associated with \code{surf} must have an edge
#'   attribute named \code{dist} containing numeric weights with no
#'   \code{NA} values.
#' @importFrom assertthat assert_that
#' @importFrom igraph E V ego distances induced_subgraph V neighborhood
#' @rdname SurfaceDisk
#' @export
SurfaceDisk <- function(surf, index, radius, max_order=NULL) {
  assertthat::assert_that(length(index) == 1)
  assertthat::assert_that(is.numeric(index), index %% 1 == 0)
  assertthat::assert_that(index >= 1, index <= length(igraph::V(surf@graph)))
  assertthat::assert_that(is.numeric(radius), radius > 0, length(radius) == 1)

  edgeWeights <- igraph::E(surf@graph)$dist
  assertthat::assert_that(!is.null(edgeWeights), !any(is.na(edgeWeights)))

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

#' values
#'
#' @param x the object to extract values from
#' @param ... extra args
#' @rdname values
#' @importMethodsFrom neuroim2 values
#' @export
setMethod("values", signature(x="ROISurface"),
          function(x, ...) {
            x@data
          })



#' @rdname values
#' @export
setMethod("values", signature(x="ROISurfaceVector"),
          function(x, ...) {
            x@data
          })


#
#' @rdname values
#' @export
setMethod("values", signature(x="NeuroSurface"),
          function(x, ...) {
            x@data
          })




#' indices
#'
#' @rdname indices
#' @export
setMethod("indices", signature(x="ROISurface"),
          function(x) {
            x@indices
          })

#' indices
#'
#' extract indices
#'
#' @param x the object to extract indices from
#'
#' @rdname indices
#' @export
setMethod("indices", signature(x="ROISurfaceVector"),
          function(x) {
            x@indices
          })

#' coords
#'
#' extract coordinates
#'
#' @param x the object to extract cooords from
#' @export
#' @rdname coords
setMethod(f="coords", signature=signature(x="ROISurface"),
          function(x) {
            x@coords
          })


#' length
#'
#' get number of elements in object
#'
#' @param x the object
#' @export
#' @rdname length
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


#' show an object
#'
#' @param object the object
#' @export
#' @rdname show
setMethod("show", signature=signature(object = "ROISurface"),
          function (object) {
            cat("\n\n\tROISurface", "\n")
            cat("\tsize: ", length(object@indices), "\n")
            cat("\tdata type:", if (is.matrix(object@data)) "matrix" else "vector", "\n" )
            cat("\tdata dim:", if (is.matrix(object@data)) dim(object@data) else length(object@data), "\n" )
            cat("\tvertex center of mass: ", colMeans(object@coords), "\n")
          })
