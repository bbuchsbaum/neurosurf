.resample <- function(x, ...) x[sample.int(length(x), ...)]


#' Create a Random Searchlight iterator for surface mesh using geodesic distance to define regions.
#'
#' @param surfgeom a surface mesh: instance of class \code{\linkS4class{SurfaceGeometry}}
#' @param radius radius of the searchlight as a geodesic distance in mm
#' @param nodeset the subset of surface node indices to use
#' @importFrom igraph neighborhood induced_subgraph
#' @export
#' @details
#'   On every call to \code{nextElem} a set of surface nodes are returned.
#'   These nodes index into the vertices of the \code{igraph} instance.
#'
#' @examples
#' file <- system.file("extdata", "std.lh.smoothwm.asc", package = "neuroim")
#' geom <- read_surf(file)
#' searchlight <- RandomSurfaceSearchlight(geom, 12)
#' nodes <- searchlight$nextElem()
#' length(nodes) > 1
#'
RandomSurfaceSearchlight <- function(surfgeom, radius=8, nodeset=NULL) {
  subgraph <- FALSE
  if (is.null(nodeset)) {
    ## use all surface nodes
    nodeset <- nodes(surfgeom)
    g <- surfgeom@graph
  } else {
    ## use supplied subset
    g <- igraph::induced_subgraph(graph(surfgeom), nodeset)
    subgraph <- TRUE
  }


  bg <- neighbor_graph(g, radius=radius)

  index <- 0

  nds <- as.vector(igraph::V(bg))
  done <- logical(length(nds))

  prog <- function() { sum(done)/length(done) }



  nextEl <- function() {
    if (!all(done)) {
      ## sample from remaining nodes
      center <- .resample(which(!done), 1)
      indices <- as.vector(igraph::neighborhood(bg, 1, nds[center])[[1]])
      indices <- indices[!done[indices]]
      done[indices] <<- TRUE

      if (subgraph) {
        ## index into to absolute graph nodes
        vout <- nodeset[indices]
        attr(vout, "center") <- nodeset[center]
        attr(vout, "center.index") <- nodeset[center]
        attr(vout, "length") <- length(vout)
        vout
      } else {
        attr(indices, "center") <- center
        attr(indices, "center.index") <- center
        attr(indices, "length") <- length(indices)
        indices
      }

    } else {
      stop('StopIteration')
    }
  }

  obj <- list(nextElem=nextEl, progress=prog)
  class(obj) <- c("RandomSurfaceSearchlight", 'abstractiter', 'iter')
  obj

}


#' SurfaceSearchlight
#'
#' Create a Searchlight iterator for surface mesh using geodesic distance to define regions.
#'
#' @param surfgeom a surface mesh: instance of class \code{SurfaceGeometry}
#' @param radius radius of the searchlight as a geodesic distance in mm
#' @param nodeset the subset of surface nodes to use
#' @importFrom igraph neighborhood induced_subgraph
#' @export
SurfaceSearchlight <- function(surfgeom, radius=8, nodeset=NULL, distance_type="geodesic") {
  assertthat::assert_that(length(radius) == 1)

  g <- if (is.null(nodeset)) {
    ## use all surface nodes
    nodeset <- nodes(surfgeom)
    subgraph <- FALSE
    neurosurf::graph(surfgeom)
  } else {
    assertthat::assert_that(length(nodeset) > 1)
    subgraph=TRUE
    igraph::induced_subgraph(neurosurf::graph(surfgeom), nodeset)
  }

  bg <- neighbor_graph(g, radius=radius, distance_type=distance_type)

  index <- 0

  nds <- igraph::V(bg)

  prog <- function() { index/length(nds) }

  getIndex <- function() { index }

  nextEl <- function() {
    if (index < length(nds)) {
      index <<- index + 1
      indices <- as.vector(igraph::neighborhood(bg, 1, nds[index])[[1]])

      if (subgraph) {
        ## index into to absolute graph nodes
        indices <- nodeset[indices]
        attr(indices, "center") <- nodeset[index]
        attr(indices, "center.index") <- nodeset[index]
        attr(indices, "length") <- length(indices)
        indices
      } else {
        attr(indices, "center") <- index
        attr(indices, "center.index") <- index
        attr(indices, "length") <- length(indices)
        indices
      }

    } else {
      stop('StopIteration')
    }
  }

  obj <- list(nextElem=nextEl, progress=prog, index=getIndex)
  class(obj) <- c("Searchlight", 'abstractiter', 'iter')
  obj

}
