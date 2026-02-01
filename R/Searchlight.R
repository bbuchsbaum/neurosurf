#' @noRd
#' @keywords internal
.resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Create a Random Searchlight iterator for surface mesh
#'
#' @description
#' Creates an iterator that randomly samples searchlight regions on a surface mesh
#' using geodesic distance to define regions.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the surface mesh.
#' @param radius Numeric, radius of the searchlight as a geodesic distance in mm.
#'   Must be a positive numeric scalar.
#' @param nodeset Integer vector, optional subset of surface node indices to use.
#'   If supplied, must contain more than one index.
#' @param as_deflist Logical, whether to return a deflist object.
#'
#' @return An iterator object of class "RandomSurfaceSearchlight".
#'
#' @details
#' On each call to \code{nextElem}, a set of surface nodes is returned.
#' These nodes index into the vertices of the \code{igraph} instance.
#' When \code{as_deflist=TRUE}, the random ordering of centers is fixed when the
#' object is created. Use \code{set.seed} before construction for reproducible
#' sequences.
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
#' geom <- read_surf(file)
#' searchlight <- RandomSurfaceSearchlight(geom, 12)
#' set.seed(42)
#' dl <- RandomSurfaceSearchlight(geom, 12, as_deflist=TRUE)
#' attr(dl[[1]], "center")
#' nodes <- searchlight$nextElem()
#' length(nodes) > 1
#' }
#'
#' @importFrom igraph neighborhood induced_subgraph
#' @importFrom deflist deflist
#' @export
RandomSurfaceSearchlight <- function(surfgeom, radius=8, nodeset=NULL, as_deflist=FALSE) {
  assertthat::assert_that(is.numeric(radius), radius > 0, length(radius) == 1)
  if (!is.null(nodeset)) assertthat::assert_that(length(nodeset) > 1)
  assertthat::assert_that(length(radius) == 1, radius > 0)
  subgraph <- FALSE
  if (is.null(nodeset)) {
    ## use all surface nodes
    nodeset <- nodes(surfgeom)
    g <- surfgeom@graph
  } else {
    assertthat::assert_that(length(nodeset) > 0)
    ## use supplied subset
    g <- igraph::induced_subgraph(graph(surfgeom), nodeset)
    subgraph <- TRUE
  }


  bg <- neighbor_graph(g, radius=radius)

  nds <- as.vector(igraph::V(bg))
  done <- logical(length(nds))

  prog <- function() { sum(done)/length(done) }

  if (as_deflist) {
    order <- sample.int(length(nds))

    # Create function to get nth element
    fun <- function(n) {
      if (n < 1 || n > length(order)) stop("Index out of bounds")
      center <- order[n]
      indices <- as.vector(igraph::neighborhood(bg, 1, nds[center])[[1]])

      if (subgraph) {
        vout <- nodeset[indices]
        attr(vout, "center") <- nodeset[center]
        attr(vout, "center.index") <- nodeset[center]
        attr(vout, "length") <- length(vout)
        vout
      } else {
        attr(indices, "center") <- nds[center]
        attr(indices, "center.index") <- nds[center]
        attr(indices, "length") <- length(indices)
        indices
      }
    }

    return(deflist::deflist(fun, len=length(order)))
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
        attr(indices, "center") <- nds[center]
        attr(indices, "center.index") <- nds[center]
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
#'   Must be a positive, length-one value.
#' @param nodeset Integer vector, optional subset of surface node indices to use.
#'   If provided, the vector must contain at least one node index.
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
#' \donttest{
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
      if (n < 1 || n > length(nds)) stop("Index out of bounds")
      indices <- as.vector(igraph::neighborhood(bg, 1, nds[n])[[1]])

      if (subgraph) {
        indices <- nodeset[indices]
        attr(indices, "center") <- nodeset[n]
        attr(indices, "center.index") <- nodeset[n]
        attr(indices, "length") <- length(indices)
        indices
      } else {
        attr(indices, "center") <- nds[n]
        attr(indices, "center.index") <- nds[n]
        attr(indices, "length") <- length(indices)
        indices
      }
    }

    return(deflist::deflist(fun, len=length(nds)))
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
        attr(indices, "center") <- nds[index]
        attr(indices, "center.index") <- nds[index]
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

#' Print Method for Searchlight Iterator
#'
#' @param x An object of class "Searchlight"
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the Searchlight object (called for side effect).
#' @method print Searchlight
#' @export
#' @importFrom crayon bold blue green red yellow style silver bgBlue white
print.Searchlight <- function(x, ...) {
  # Check if crayon is available
  has_crayon <- requireNamespace("crayon", quietly = TRUE)

  # Define styling functions
  header <- if(has_crayon) function(txt) crayon::bgBlue(crayon::white(crayon::bold(txt))) else function(txt) txt
  title <- if(has_crayon) function(txt) crayon::blue(crayon::bold(txt)) else function(txt) txt
  subtitle <- if(has_crayon) function(txt) crayon::green(txt) else function(txt) txt
  highlight <- if(has_crayon) function(txt) crayon::yellow(txt) else function(txt) txt
  normal <- if(has_crayon) function(txt) crayon::silver(txt) else function(txt) txt

  # Get available info
  progress_val <- tryCatch(x$progress(), error = function(e) NA)
  current_index <- tryCatch(x$index(), error = function(e) NA) # Renamed from getIndex for clarity if needed

  cat("\n")
  cat(header(" Surface Searchlight Iterator "), "\n\n")

  cat(title("  Iterator Status:"), "\n")
  if (!is.na(progress_val)) {
      # Attempt to get total number of steps if possible (might require modification)
      # total_nodes <- tryCatch(x$getTotal(), error = function(e) NA) # Hypothetical function
      # if (!is.na(total_nodes) && !is.na(current_index)) {
      #    cat("  ", subtitle("Current Step:"), "", highlight(format(current_index, big.mark=",")), " / ", highlight(format(total_nodes, big.mark=",")), "\n", sep="")
      # } else {
      #    cat("  ", subtitle("Current Step:"), "", highlight(format(current_index, big.mark=",")), " (Total unknown)", "\n", sep="")
      # }
      cat("  ", subtitle("Progress:"), "   ", highlight(paste0(round(progress_val * 100, 1), "%")), "\n", sep="")
  } else {
      cat("  ", normal("(Progress information not available)"), "\n")
  }
  cat("\n")
  cat(normal("  (Note: Radius, distance type, and geometry details are not shown in this view)"), "\n")
  cat("\n")

  invisible(x)
}
