#' @noRd
.resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Create a Random Searchlight iterator for surface mesh
#'
#' @description
#' Creates an iterator that randomly samples searchlight regions on a surface mesh
#' using geodesic distance to define regions.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the surface mesh.
#' @param radius Numeric, radius of the searchlight as a geodesic distance in mm.
#' @param nodeset Integer vector, optional subset of surface node indices to use.
#' @param as_deflist Logical, whether to return a deflist object.
#'
#' @return An iterator object of class "RandomSurfaceSearchlight".
#'
#' @details
#' On each call to \code{nextElem}, a set of surface nodes is returned.
#' These nodes index into the vertices of the \code{igraph} instance.
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
#' geom <- read_surf(file)
#' searchlight <- RandomSurfaceSearchlight(geom, 12)
#' nodes <- searchlight$nextElem()
#' length(nodes) > 1
#' }
#'
#' @importFrom igraph neighborhood induced_subgraph
#' @export
RandomSurfaceSearchlight <- function(surfgeom, radius=8, nodeset=NULL, as_deflist=FALSE) {
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

  if (as_deflist) {
    # Create function to get nth element
    fun <- function(n) {
      if (n > length(nds)) stop("Index out of bounds")
      center <- which(!done)[n]
      indices <- as.vector(igraph::neighborhood(bg, 1, nds[center])[[1]])
      indices <- indices[!done[indices]]
      
      if (subgraph) {
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
    }
    
    return(deflist(fun, len=sum(!done)))
  }

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
#' @description
#' Creates an iterator that systematically samples searchlight regions on a surface mesh
#' using geodesic distance to define regions.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the surface mesh.
#' @param radius Numeric, radius of the searchlight as a geodesic distance in mm.
#' @param nodeset Integer vector, optional subset of surface node indices to use.
#' @param distance_type Character, the distance metric to use: "euclidean", "geodesic", or "spherical".
#' @param as_deflist Logical, whether to return a deflist object.
#'
#' @return An iterator object of class "Searchlight".
#'
#' @details
#' This function creates a systematic searchlight iterator, which visits each node
#' of the surface mesh in order. The searchlight region for each node is defined
#' by the specified radius and distance metric.
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
#' geom <- read_surf(file)
#' searchlight <- SurfaceSearchlight(geom, 12, distance_type = "geodesic")
#' nodes <- searchlight$nextElem()
#' }
#'
#' @importFrom igraph neighborhood induced_subgraph
#' @export
SurfaceSearchlight <- function(surfgeom, radius=8, nodeset=NULL, distance_type=c("euclidean", "geodesic", "spherical"), as_deflist=FALSE) {
  assertthat::assert_that(length(radius) == 1)
  distance_type <- match.arg(distance_type)
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

  if (as_deflist) {
    # Create function to get nth element
    fun <- function(n) {
      if (n > length(nds)) stop("Index out of bounds")
      indices <- as.vector(igraph::neighborhood(bg, 1, nds[n])[[1]])
      
      if (subgraph) {
        indices <- nodeset[indices]
        attr(indices, "center") <- nodeset[n]
        attr(indices, "center.index") <- nodeset[n]
        attr(indices, "length") <- length(indices)
        indices
      } else {
        attr(indices, "center") <- n
        attr(indices, "center.index") <- n
        attr(indices, "length") <- length(indices)
        indices
      }
    }
    
    return(deflist(fun, len=length(nds)))
  }

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
