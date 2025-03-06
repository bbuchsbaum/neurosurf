#' Constructor for NeuroSurfaceSource
#'
#' @param surface_geom the name of the file containing the surface geometry or a \code{SurfaceGeometry} instance
#' @param surface_data_name the name of the file containing the data values to be mapped to the surface.
#' @param colind the subset of column indices to load from surface data matrix (if provided)
#' @param nodeind the subset of node indices to load from surface data matrix (if provided)
#' @return An object of class \code{\linkS4class{NeuroSurfaceSource}} or \code{\linkS4class{NeuroSurfaceVectorSource}} 
#'         that contains information about the surface geometry and associated data. If the data has multiple columns 
#'         (colind > 1), a \code{NeuroSurfaceVectorSource} is returned; otherwise, a \code{NeuroSurfaceSource} is returned.
#'         These objects can be used to load and map neuroimaging data onto brain surfaces.
#' @export
#' @rdname NeuroSurfaceSource-class
NeuroSurfaceSource <- function(surface_geom, surface_data_name, colind=NULL, nodeind=NULL) {
  if (is.character(surface_geom)) {
    assert_that(file.exists(surface_geom))
    src <- SurfaceGeometrySource(surface_geom)
    surface_geom <- load_data(src)
  }

  data_meta_info <- .readHeader(surface_data_name)

  if (is.null(colind)) {
    colind <- 1:data_meta_info@nels
  }

  if (is.null(nodeind)) {
    nodeind <- nodes(surface_geom)
  }

  if (length(colind) > 1 && data_meta_info@nels > 1) {
    new("NeuroSurfaceVectorSource", geometry=surface_geom,
        data_meta_info=data_meta_info,
        colind=as.integer(colind),
        nodeind=as.integer(nodeind))
  } else {
    new("NeuroSurfaceSource", geometry=surface_geom,
        data_meta_info=data_meta_info,
        colind=as.integer(colind),
        nodeind=as.integer(nodeind))
  }

}


#' @rdname coords
#' @export
setMethod(f="coords", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            coords(geometry(x))
          })



#' @rdname coords
#' @export
setMethod(f="coords", signature=c("NeuroSurface"),
          def=function(x) {
            coords(geometry(x))
          })



#' @rdname geometry
#' @export
setMethod(f="geometry", signature=c("NeuroSurface"),
          def=function(x) {
            x@geometry
          })

#' @rdname geometry
#' @export
setMethod(f="geometry", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            x@geometry
          })

#' @rdname as.matrix
#' @export
setMethod(f="as.matrix", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            as.matrix(x@data)
          })



#' @export
#' @rdname vertices
setMethod(f="vertices", signature=c("NeuroSurface"),
          def=function(x) {
            vertices(x@geometry)
          })


#' @rdname vertices
#' @param indices a vector of indices specifying the valid surface nodes.
#' @export
setMethod(f="vertices", signature=c("NeuroSurfaceVector"),
          def=function(x, indices) {
            callGeneric(x@geometry, indices)
          })


#' @importMethodsFrom neuroim2 indices
#' @export
#' @rdname indices
setMethod(f="indices", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            x@indices
          })


#' @importMethodsFrom neuroim2 indices
#' @export
#' @rdname indices
setMethod(f="indices", signature=c("NeuroSurface"),
          def=function(x) {
            x@indices
          })


#' @export
#' @rdname nodes
setMethod(f="nodes", signature=c("NeuroSurface"),
          def=function(x) {
            callGeneric(x@geometry)
          })


#' @export
#' @rdname nodes
setMethod(f="nodes", signature=c(x="NeuroSurfaceVector"),
          def=function(x) {
            callGeneric(x@geometry)
          })



#' connected components on a surface
#'
#' @export
#' @importMethodsFrom neuroim2 conn_comp
#' @param x the object
#' @param threshold the two-element threshold range to use to define
#' connected components
#' @param index of the data vector to find connected components on
#' @rdname conn_comp
setMethod(f="conn_comp", signature=c(x="NeuroSurfaceVector"),
          def=function(x, threshold, index=1) {
            vals <- x@data[,index]
            surf <- NeuroSurface(x@geometry, indices=x@indices, vals)
            callGeneric(surf, threshold)

          })


#' @export
#' @param index the index/column of the underlying data matrix to cluster
#' @rdname cluster_threshold
setMethod(f="cluster_threshold", signature=c(x="NeuroSurfaceVector"),
          def=function(x, threshold, size=10, index=1) {
            vals <- x@data[,index]
            surf <- NeuroSurface(x@geometry, indices=x@indices, vals)
            ret <- conn_comp(surf, threshold)
            surf@vals[ret@size < size] <- 0
            surf
          })





#' @export
#' @rdname cluster_threshold
setMethod(f="cluster_threshold", signature=c(x="NeuroSurface"),
          def=function(x, threshold, size=10) {
            ret <- conn_comp(x, threshold)
            x@data[ret$size < size] <- 0
            x
          })


#' Compute Connected Components on a Surface
#'
#' This method identifies connected components on a NeuroSurface object based on a given threshold.
#'
#' @param x A \code{NeuroSurface} object representing the surface data.
#' @param threshold A numeric vector of length 2 specifying the lower and upper thresholds for including vertices in the components.
#'
#' @return A list containing two \code{NeuroSurface} objects:
#'   \item{index}{A \code{NeuroSurface} object where each vertex is labeled with its component index.}
#'   \item{size}{A \code{NeuroSurface} object where each vertex is labeled with the size of its component.}
#'
#' @details
#' This method computes connected components on the surface by:
#' \enumerate{
#'   \item Thresholding the surface data using the provided threshold values.
#'   \item Creating a subgraph of the surface mesh containing only the vertices that pass the threshold.
#'   \item Identifying connected components in this subgraph.
#'   \item Assigning component indices and sizes to the original vertices.
#' }
#' Vertices that do not pass the threshold are assigned a value of 0 in both output surfaces.
#'
#' @examples
#' # Load a sample surface from the package
#' surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
#' surf_geom <- readAsc(surf_file)
#' 
#' # Create random data for the surface with some clusters
#' n_vertices <- nrow(coords(surf_geom))
#' set.seed(123)
#' random_data <- rnorm(n_vertices, mean = 0, sd = 1)
#' 
#' # Create a few clusters of higher values
#' cluster_centers <- sample(1:n_vertices, 5)
#' g <- graph(surf_geom)
#' 
#' # For each cluster center, set nearby vertices to higher values
#' for (center in cluster_centers) {
#'   # Get neighbors within 2 steps
#'   neighbors <- unlist(igraph::neighborhood(g, 2, center))
#'   random_data[neighbors] <- random_data[neighbors] + 2
#' }
#' 
#' # Create a NeuroSurface object
#' neuro_surf <- NeuroSurface(geometry = surf_geom, 
#'                           indices = 1:n_vertices,
#'                           data = random_data)
#' 
#' # Find connected components with threshold c(-Inf, 1.5)
#' # This will identify clusters where values are >= 1.5
#' components <- conn_comp(neuro_surf, c(-Inf, 1.5))
#' 
#' # Check the number of components found
#' max(series(components$index))
#' 
#' # Check the size of the largest component
#' max(series(components$size))
#' 
#' # Count vertices in components of size >= 10
#' sum(series(components$size) >= 10)
#'
#' @seealso \code{\link{NeuroSurface}}, \code{\link[igraph]{components}}
#'
#' @importMethodsFrom neuroim2 conn_comp
#' @importFrom igraph induced_subgraph components V
#' @export
#' @rdname conn_comp
setMethod(f="conn_comp", signature=c(x="NeuroSurface"),
          def=function(x, threshold) {
            keep <- x@data <= threshold[1] | x@data >= threshold[2]
            vs <- V(x@geometry@graph)[keep]
            sg <- igraph::induced_subgraph(x@geometry@graph, vs)
            comps <- igraph::components(sg)

            memb <- numeric(length(keep))
            memb[keep] <- comps$membership

            sizes <- numeric(length(keep))
            sizes[keep] <- comps$csize[comps$membership]

            index_surf <- NeuroSurface(x@geometry, x@indices, memb)
            size_surf <- NeuroSurface(x@geometry, x@indices, sizes)

            list(index=index_surf, size=size_surf)
          })


#' extract a series of values for a surface vector
#'
#' @rdname series
#' @param x the object to extract series from
#' @param i the indices of the series set
#' @importFrom Matrix Matrix
#' @importFrom Matrix t
#' @importMethodsFrom neuroim2 series
#' @return a class of type \code{Matrix}
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="numeric"),
          def=function(x, i) {
            Matrix::t(x@data[i,,drop=FALSE])
          })

#' series_roi
#'
#' @rdname series_roi
#' @param x the object o extract series from
#' @param i the set of indices to extract
#' @return a class of type \code{ROISurfaceVector}
#' @importMethodsFrom neuroim2 series_roi
#' @export
setMethod("series_roi", signature(x="NeuroSurfaceVector", i="numeric"),
          def=function(x, i) {
            m <- as.matrix(series(x,i))
            ROISurfaceVector(geometry=x@geometry, indices=i, data=m)
          })


#' @rdname series
#' @importFrom Matrix Matrix
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="integer"),
          def=function(x, i) {
            Matrix::t(x@data[i,])
          })

#' @rdname series
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="ROISurface"),
          def=function(x, i) {
            callGeneric(x, indices(i))
          })



#' @rdname series_roi
#' @importMethodsFrom neuroim2 series_roi
#' @export
setMethod("series_roi", signature(x="NeuroSurfaceVector", i="ROISurface"),
          def=function(x, i) {
            mat <- series(x, indices(i))
            ROISurfaceVector(x@geometry, indices(i), as.matrix(mat))
          })


#' @rdname series
#' @export
setMethod("series", signature(x="NeuroSurface", i="numeric"),
          def=function(x, i) {
            Matrix::t(x@data[i])
          })


#' map_values
#'
#' map data using a key -> value lookup table
#'
#' @importMethodsFrom neuroim2 map_values
#' @export
#' @param x the object to map over
#' @param lookup the lookup table
#' @rdname map_values
setMethod("map_values", signature(x="NeuroSurface", lookup="list"),
          def=function(x,lookup) {
            outv <- lookup[as.vector(x@data)]
            NeuroSurface(geometry=x@geometry, indices=x@indices, data=outv)

          })



#' @importMethodsFrom neuroim2 map_values
#' @export
#' @rdname map_values
setMethod("map_values", signature(x="NeuroSurface", lookup="matrix"),
          def=function(x,lookup) {
            if (ncol(lookup) != 2) {
              stop("fill: lookup matrix must have two columns: column 1 is key, column 2 is value")
            } else if (nrow(lookup) < 1) {
              stop("fill: lookup matrix have at least one row")
            }

            m <- match(as.vector(x@data), lookup[,1])
            outv <- lookup[m,2]

            outv[is.na(outv)] <- 0
            NeuroSurface(geometry=x@geometry, indices=x@indices, data=outv)

          })


#' convert \code{NeuroSurface} instance to vector
#'
#' @param x the object
#' @export
setMethod(f="as.vector", signature=signature(x = "NeuroSurface"), def=function(x) as(x, "vector"))

#' convert from \code{NeuroSurface} to \code{vector}
#'
#' @rdname as-methods
#' @name as
setAs(from="NeuroSurface", to="vector", def=function(from) as.vector(from@data))




#' @export
#' @rdname graph
setMethod("graph", signature(x="NeuroSurface"),
          def=function(x,...) {
            callGeneric(x@geometry)
          })




#' @export
#' @rdname graph
setMethod("graph", signature(x="NeuroSurfaceVector"),
          def=function(x, ...) {
            callGeneric(x@geometry)
          })



#' NeuroSurfaceVector
#'
#' construct a new NeuroSurfaceVector
#' @param geometry a \code{SurfaceGeometry} instance
#' @param indices an integer vector specifying the valid surface nodes.
#' @param mat a \code{matrix} of data values (rows=nodes, columns=variables)
#' @export
NeuroSurfaceVector <- function(geometry, indices, mat) {
  new("NeuroSurfaceVector", geometry=geometry, indices=as.integer(indices),
      data=Matrix::Matrix(mat))

}


#' Construct a NeuroSurface Object
#'
#' This function creates a new NeuroSurface object, which represents a single set of data values
#' associated with nodes on a surface geometry.
#'
#' @param geometry A \code{SurfaceGeometry} object representing the underlying surface structure.
#' @param indices An integer vector specifying the indices of valid surface nodes.
#' @param data A numeric vector of data values corresponding to the surface nodes.
#'
#' @return A new object of class \code{NeuroSurface} containing:
#'   \item{geometry}{The input \code{SurfaceGeometry} object.}
#'   \item{indices}{The input integer vector of valid node indices.}
#'   \item{data}{The input numeric vector of data values.}
#'
#' @details
#' The NeuroSurface object is designed to store and manipulate a single set of data values
#' associated with nodes on a surface. This can represent various neuroimaging measures
#' such as cortical thickness, functional activation, or any other node-wise metric.
#'
#' The length of the \code{data} vector should match the length of the \code{indices} vector.
#' Nodes not included in the \code{indices} vector are considered to have no data or to be invalid.
#'
#' @examples
#' \dontrun{
#' # Assuming 'surf_geom' is a pre-existing SurfaceGeometry object
#' indices <- 1:1000  # Example indices
#' data_values <- rnorm(1000)  # Example data
#' neuro_surf <- NeuroSurface(surf_geom, indices, data_values)
#' }
#'
#' @seealso \code{\link{SurfaceGeometry}}, \code{\link{NeuroSurfaceVector}}
#'
#' @export
NeuroSurface <- function(geometry, indices, data) {
  new("NeuroSurface", geometry=geometry, indices=as.integer(indices),
      data=data)

}



#' ColorMappedNeuroSurface
#'
#' This function creates a ColorMappedNeuroSurface object, which represents a single set of data values
#' associated with nodes on a surface geometry, with color mapping applied.
#'
#' @param geometry A \code{SurfaceGeometry} object representing the underlying surface structure.
#' @param indices An integer vector specifying the indices of valid surface nodes.
#' @param data A numeric vector of data values corresponding to the surface nodes.
#' @param cmap A character string specifying the colormap to use for mapping the data values.
#' @param irange A numeric vector of length 2 specifying the range of values to map.
#' @param thresh A numeric value specifying the threshold for the colormap. 
ColorMappedNeuroSurface <- function(geometry, indices, data, cmap, irange, thresh) {
  new("ColorMappedNeuroSurface", geometry=geometry, indices=as.integer(indices),
      data=data, cmap=cmap, irange=irange, thresh=thresh)
}


#' @rdname show
#' @export
setMethod(f="show", signature=signature("NeuroSurfaceVector"),
          def=function(object) {
            cat("NeuroSurfaceVector \n")
            cat("  num vertices: ", length(nodes(object@geometry)), "\n")
            cat("  num nonzero indices:", length(object@indices), "\n")
            cat("  num columns:", ncol(object@data), "\n")

          })

#' @rdname show
#' @export
setMethod(f="show", signature=signature("NeuroSurface"),
          def=function(object) {
            cat("NeuroSurface \n")
            cat("  num vertices: ", length(nodes(object@geometry)), "\n")
            cat("  num nonzero indices:", length(object@indices), "\n")
          })





#' @rdname left
#' @export
setMethod(f="left", signature=c(x="BilatNeuroSurfaceVector"),
          def=function(x) {
            x@left
          })



#' @rdname right
#' @export
setMethod(f="right", signature=c(x="BilatNeuroSurfaceVector"),
          def=function(x) {
            x@right
          })



#' @keywords internal
normalize <- function(vals) (vals - min(vals))/(max(vals)-min(vals))



#' extractor
#' @export
#' @param x the object
#' @param i first index
setMethod(f="[[", signature=signature(x = "NeuroSurfaceVector", i = "numeric"),
          def=function (x, i) {
            x@data[,i]

          })



#' extractor
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "numeric", j = "numeric", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[i,j]

          })

#' extractor
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "missing", j = "numeric", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[,j]
          })



#' extractor
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "numeric", j = "missing", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[i,]
          })

#' extractor
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "missing", j = "missing", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[,]
          })

#' @export
setAs(from="BilatNeuroSurfaceVector", to="matrix",
      def=function(from) {
        mat1 <- left(from)[]
        mat2 <- right(from)[]
        out <- rbind(mat1,mat2)
        attr(out, "coords") <- rbind(coords(left(from)), coords(right(from)))
        attr(out, "indices") <- c(indices(left(from)), indices(right(from)))
        attr(out, "hemi") <- c(rep(1, nrow(mat1)), rep(2, nrow(mat2)))
        out

      })


#' @export
#' @rdname as.matrix
setMethod("as.matrix", signature(x = "BilatNeuroSurfaceVector"), function(x) as(x, "matrix"))






