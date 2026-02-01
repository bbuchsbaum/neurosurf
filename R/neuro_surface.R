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


#' @rdname coords-methods
#' @export
setMethod(f="coords", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            coords(geometry(x))
          })



#' @rdname coords-methods
#' @export
setMethod(f="coords", signature=c("NeuroSurface"),
          def=function(x) {
            coords(geometry(x))
          })



#' @rdname geometry-methods
#' @export
setMethod(f="geometry", signature=c("NeuroSurface"),
          def=function(x) {
            x@geometry
          })

#' @rdname geometry-methods
#' @export
setMethod(f="geometry", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            x@geometry
          })

#' Convert Surface Data to Matrix
#'
#' @description
#' Converts surface data to a matrix representation.
#'
#' @param x the object to convert to a matrix
#' @return A numeric matrix with vertices as rows and time points/features as columns
#' @rdname as.matrix-methods
#' @export
setMethod(f="as.matrix", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            as.matrix(x@data)
          })




#' @export
#' @rdname vertices-methods
setMethod(f="vertices", signature=c("NeuroSurface"),
          def=function(x) {
            vertices(x@geometry)
          })


#' @rdname vertices-methods
#' @param indices a vector of indices specifying the valid surface nodes.
#' @export
setMethod(f="vertices", signature=c("NeuroSurfaceVector"),
          def=function(x, indices) {
            callGeneric(x@geometry, indices)
          })


#' @export
#' @rdname faces-methods
setMethod(f="faces", signature=c("NeuroSurface"),
          def=function(x) {
            faces(x@geometry)
          })


#' @export
#' @rdname faces-methods
setMethod(f="faces", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            faces(x@geometry)
          })


#' @importMethodsFrom neuroim2 indices
#' @export
#' @rdname indices-methods
setMethod(f="indices", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            x@indices
          })


#' @importMethodsFrom neuroim2 indices
#' @export
#' @rdname indices-methods
setMethod(f="indices", signature=c("NeuroSurface"),
          def=function(x) {
            x@indices
          })


#' @export
#' @rdname nodes-methods
setMethod(f="nodes", signature=c("NeuroSurface"),
          def=function(x) {
            callGeneric(x@geometry)
          })


#' @export
#' @rdname nodes-methods
setMethod(f="nodes", signature=c(x="NeuroSurfaceVector"),
          def=function(x) {
            callGeneric(x@geometry)
          })



#' @export
#' @rdname conn_comp-methods
#' @param index the index/column of the underlying data matrix to cluster
setMethod(f="conn_comp", signature=c(x="NeuroSurfaceVector"),
          def=function(x, threshold, index=1) {
            vals <- x@data[,index]
            surf <- NeuroSurface(x@geometry, indices=x@indices, vals)
            callGeneric(surf, threshold)

          })


#' @export
#' @param threshold the two-element threshold range to use to define connected components
#' @param size the minimum size of the connected components to keep
#' @param index the index/column of the underlying data matrix to cluster
#' @rdname cluster_threshold-methods
setMethod(f="cluster_threshold", signature=c(x="NeuroSurfaceVector"),
          def=function(x, threshold, size=10, index=1) {
            # Create a temporary NeuroSurface for component analysis
            vals <- x@data[,index]
            surf <- NeuroSurface(x@geometry, indices=x@indices, vals)
            ret <- conn_comp(surf, threshold)
            surf@data[ret$size@data < size] <- 0
            surf
})




#' @export
#' @rdname cluster_threshold-methods
setMethod(f="cluster_threshold", signature=c(x="NeuroSurface"),
          def=function(x, threshold, size=10) {

            ret <- conn_comp(x, threshold)
            x@data[ret$size@data < size] <- 0
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
#' surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
#' surf_geom <- read_surf_geometry(surf_file)
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
#' max(series(components$index, seq_len(n_vertices)))
#'
#' # Check the size of the largest component
#' max(series(components$size, seq_len(n_vertices)))
#'
#' # Count vertices in components of size >= 10
#' sum(series(components$size, seq_len(n_vertices)) >= 10)
#'
#' @seealso \code{\link{NeuroSurface}}, \code{\link[igraph]{components}}
#'
#' @importMethodsFrom neuroim2 conn_comp
#' @importFrom igraph induced_subgraph components V
#' @export
#' @rdname conn_comp-methods
setMethod(f="conn_comp", signature=c(x="NeuroSurface"),
          def=function(x, threshold) {
            keep <- x@data <= threshold[1] | x@data >= threshold[2]
            g <- graph(x@geometry)
            vs <- igraph::V(g)[keep]
            sg <- igraph::induced_subgraph(g, vs)
            comps <- igraph::components(sg)

            memb <- numeric(length(keep))
            memb[keep] <- comps$membership

            sizes <- numeric(length(keep))
            sizes[keep] <- comps$csize[comps$membership]

            index_surf <- NeuroSurface(x@geometry, x@indices, memb)
            size_surf <- NeuroSurface(x@geometry, x@indices, sizes)

            list(index=index_surf, size=size_surf)
          })

#' Extract Time Series from Surface Vector
#'
#' @description
#' Extracts time series data from specific vertices in a surface vector.
#'
#' @rdname series-methods
#' @param x the object to extract the series from
#' @param i the indices of the series to extract
#' @return A matrix where columns are time points and rows are vertex indices
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="numeric"),
          def=function(x, i) {
            Matrix::t(x@data[i,,drop=FALSE])
          })

#' Extract ROI Time Series from Surface Vector
#'
#' @description
#' Extracts time series data from a region of interest in a surface vector.
#'
#' @rdname series_roi-methods
#' @param x the object to extract the series from
#' @param i the indices of the series to extract
#' @return An \code{\linkS4class{ROISurfaceVector}} containing the extracted time series
#' @importMethodsFrom neuroim2 series_roi
#' @export
setMethod("series_roi", signature(x="NeuroSurfaceVector", i="numeric"),
          def=function(x, i) {
            m <- as.matrix(series(x,i))
            ROISurfaceVector(geometry=x@geometry, indices=i, data=m)
          })


#' @rdname series-methods
#' @importFrom Matrix Matrix
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="integer"),
          def=function(x, i) {
            Matrix::t(x@data[i,])
          })

#' @rdname series-methods
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="ROISurface"),
          def=function(x, i) {
            callGeneric(x, indices(i))
          })



#' @rdname series_roi-methods
#' @importMethodsFrom neuroim2 series_roi
#' @export
setMethod("series_roi", signature(x="NeuroSurfaceVector", i="ROISurface"),
          def=function(x, i) {
            mat <- series(x, indices(i))
            ROISurfaceVector(x@geometry, indices(i), as.matrix(mat))
          })


#' @rdname series-methods
#' @export
setMethod("series", signature(x="NeuroSurface", i="numeric"),
          def=function(x, i) {
            Matrix::t(x@data[i])
          })

#' Map Values for NeuroSurface with List Lookup
#'
#' @param x a \code{NeuroSurface} object
#' @param lookup a list of values to map
#' @return A \code{NeuroSurface} object with remapped values
#' @rdname map_values-NeuroSurface-list-method
#' @export
setMethod("map_values", signature(x="NeuroSurface", lookup="list"),
          def=function(x,lookup) {
            outv <- lookup[as.vector(x@data)]
            NeuroSurface(geometry=x@geometry, indices=x@indices, data=outv)

          })



#' Map Values for NeuroSurface with Matrix Lookup
#'
#' @importMethodsFrom neuroim2 map_values
#' @param x a \code{NeuroSurface} object
#' @param lookup a \code{matrix} with two columns: the first column is the key, and the second column is the value
#' @return A \code{NeuroSurface} object with remapped values
#' @export
#' @rdname map_values-NeuroSurface-matrix-method
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


#' Convert Surface Data to Vector
#'
#' @description
#' Converts surface data to a numeric vector.
#'
#' @param x the object to convert to a vector
#' @return A numeric vector of surface data values
#' @export
#' @rdname as.vector-methods
setMethod(f="as.vector", signature=signature(x = "NeuroSurface"), def=function(x) as(x, "vector"))

#' Coercion Methods for NeuroSurface Objects
#'
#' Methods to convert NeuroSurface objects to other types.
#'
#' @name as
#' @return The converted object (e.g., a numeric vector when converting to "vector").
#'
#' @export
#' @rdname as-methods
setAs(from="NeuroSurface", to="vector", def=function(from) as.vector(from@data))




#' @export
#' @rdname graph-methods
setMethod("graph", signature(x="NeuroSurface"),
          def=function(x,...) {
            callGeneric(x@geometry)
          })




#' @export
#' @rdname graph-methods
setMethod("graph", signature(x="NeuroSurfaceVector"),
          def=function(x, ...) {
            callGeneric(x@geometry)
          })



#' NeuroSurfaceVector
#'
#' construct a new NeuroSurfaceVector
#'
#' @param geometry a \code{SurfaceGeometry} or \code{SurfaceSet} instance
#' @param indices an integer vector specifying the valid surface nodes.
#' @param mat a \code{matrix} of data values (rows=nodes, columns=variables)
#' @return A \code{\linkS4class{NeuroSurfaceVector}} object containing the geometry, node indices, and data matrix.
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
#' @param geometry A \code{SurfaceGeometry} or \code{SurfaceSet} object representing the underlying surface structure.
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
#' \donttest{
#' surf_geom <- example_surface_geometry()
#' indices <- seq_len(nrow(coords(surf_geom)))  # all vertices
#' data_values <- rnorm(length(indices))        # Example data
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
#' associated with nodes on a surface geometry, with pre-defined color mapping parameters.
#'
#' @param geometry A \code{SurfaceGeometry} object representing the underlying surface structure.
#' @param indices An integer vector specifying the indices of valid surface nodes.
#' @param data A numeric vector of data values corresponding to the surface nodes.
#' @param cmap A character string specifying the colormap to use for mapping the data values.
#' @param irange A numeric vector of length 2 specifying the range of values to map.
#' @param thresh A numeric value specifying the threshold for the colormap.
#' @return An instance of class \code{ColorMappedNeuroSurface}.
#' @details This object bundles the surface geometry, data, and specific color mapping
#'   parameters ('cmap', 'irange', 'thresh'). This is useful for ensuring consistent
#'   visualization across different plots or for saving a predefined view. The actual
#'   application of the color map happens during rendering (e.g., when using 'plot()'').
#'
#' @examples
#' # Load a sample surface geometry
#' surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
#' surf_geom <- read_surf_geometry(surf_file)
#'
#' # Get vertex count and generate some random data
#' n_verts <- nrow(coords(surf_geom))
#' set.seed(123)
#' vertex_data <- rnorm(n_verts)
#'
#' # Define indices (all vertices in this case)
#' vertex_indices <- 1:n_verts
#'
#' # Define color mapping parameters
#' my_cmap <- colorRampPalette(c("blue", "white", "red"))(256) # Blue-white-red colormap
#' my_irange <- c(-2, 2) # Map data values from -2 to 2 onto the colormap
#' my_thresh <- c(-1, 1) # Define thresholds (e.g., for transparency later)
#'
#' # Create the ColorMappedNeuroSurface object
#' mapped_surf <- ColorMappedNeuroSurface(geometry = surf_geom,
#'                                        indices = vertex_indices,
#'                                        data = vertex_data,
#'                                        cmap = my_cmap,
#'                                        irange = my_irange,
#'                                        thresh = my_thresh)
#'
#' # Print the object summary
#' print(mapped_surf)
#'
#' # The object can now be plotted, and the plotting function will use
#' # the stored cmap, irange, and thresh parameters by default.
#' # plot(mapped_surf) # Requires rgl package
#'
#' @seealso \code{\link{SurfaceGeometry}}, \code{\link{NeuroSurface}}
#' @export
ColorMappedNeuroSurface <- function(geometry, indices, data, cmap, irange, thresh) {
  new("ColorMappedNeuroSurface", geometry=geometry, indices=as.integer(indices),
      data=data, cmap=cmap, irange=irange, thresh=thresh)
}


#' VertexColoredNeuroSurface
#'
#' This function creates a VertexColoredNeuroSurface object, which represents a surface
#' with explicit colors assigned to each vertex.
#'
#' @param geometry A \code{SurfaceGeometry} object representing the underlying surface structure.
#' @param indices An integer vector specifying the indices of valid surface nodes.
#' @param colors A character vector of hex color codes for each vertex.
#' @param data An optional numeric vector of data values. If not provided, defaults to zeros.
#'   This parameter exists for compatibility with the parent NeuroSurface class but is not
#'   used for coloring (colors are specified directly via the \code{colors} parameter).
#'
#' @details This object represents a surface where each vertex has an explicitly assigned color,
#'   bypassing any data-to-color mapping. This is useful when you want direct control over
#'   vertex colors, such as when displaying parcellation results or pre-computed color schemes.
#'
#' @examples
#' \donttest{
#' # Load a sample surface geometry
#' surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
#' surf_geom <- read_surf_geometry(surf_file)
#'
#' # Get first 100 vertices for this example
#' n_verts <- min(100, nrow(coords(surf_geom)))
#' vertex_indices <- 1:n_verts
#'
#' # Create colors based on vertex coordinates (x-position)
#' x_coords <- coords(surf_geom)[vertex_indices, 1]
#' vertex_colors <- ifelse(x_coords > median(x_coords), "#FF6B6B", "#4ECDC4")
#'
#' # Create the VertexColoredNeuroSurface object
#' colored_surf <- VertexColoredNeuroSurface(geometry = surf_geom,
#'                                           indices = vertex_indices,
#'                                           colors = vertex_colors)
#'
#' # Print the object summary
#' print(colored_surf)
#'
#' # The object can now be plotted with the specified colors
#' # plot(colored_surf) # Requires rgl package
#' }
#'
#' @return A \code{VertexColoredNeuroSurface} object containing the surface geometry
#'   with explicit vertex colors.
#' @seealso \code{\link{SurfaceGeometry}}, \code{\link{NeuroSurface}}, \code{\link{ColorMappedNeuroSurface}}
#' @export
VertexColoredNeuroSurface <- function(geometry, indices, colors, data = NULL) {
  if (is.null(data)) {
    data <- rep(0, length(indices))
  }
  new("VertexColoredNeuroSurface", geometry=geometry, indices=as.integer(indices),
      data=data, colors=colors)
}


#' @rdname show-methods
#' @export
#' @importFrom crayon bold blue green red yellow style silver bgBlue white
setMethod(f="show", signature=signature("NeuroSurfaceVector"),
          def=function(object) {
            # Check if crayon is available
            has_crayon <- requireNamespace("crayon", quietly = TRUE)

            # Define styling functions
            header <- if(has_crayon) function(x) crayon::bgBlue(crayon::white(crayon::bold(x))) else function(x) x
            title <- if(has_crayon) function(x) crayon::blue(crayon::bold(x)) else function(x) x
            subtitle <- if(has_crayon) function(x) crayon::green(x) else function(x) x
            highlight <- if(has_crayon) function(x) crayon::yellow(x) else function(x) x
            normal <- if(has_crayon) function(x) crayon::silver(x) else function(x) x

            num_geom_verts <- length(nodes(object@geometry))
            num_data_indices <- length(object@indices)
            num_cols <- ncol(object@data)
            data_dims <- dim(object@data)

            cat("\n")
            cat(header(" NeuroSurfaceVector "), "\n\n")

            cat(title("  Geometry & Data Mapping:"), "\n")
            cat("  ", subtitle("Hemisphere:"), "         ", highlight(object@geometry@hemi), "\n", sep="")
            cat("  ", subtitle("Total Vertices:"), "   ", highlight(format(num_geom_verts, big.mark=",")), "\n", sep="")
            cat("  ", subtitle("Vertices w/ Data:"), "", highlight(format(num_data_indices, big.mark=",")), "\n", sep="")
            cat("\n")

            cat(title("  Data Matrix Information:"), "\n")
            cat("  ", subtitle("Number of Vectors:"), "", highlight(num_cols), "\n", sep="")
            cat("  ", subtitle("Data Dimensions:"), "  ", highlight(paste0("[", data_dims[1], " rows x ", data_dims[2], " cols]")), "\n", sep="")
            cat("  ", subtitle("Matrix Class:"), "     ", highlight(class(object@data)[1]), "\n", sep="")
            cat("\n")
          })

#' @rdname show-methods
#' @export
#' @importFrom crayon bold blue green red yellow style silver bgBlue white
setMethod(f="show", signature=signature("NeuroSurface"),
          def=function(object) {
            # Check if crayon is available
            has_crayon <- requireNamespace("crayon", quietly = TRUE)

            # Define styling functions
            header <- if(has_crayon) function(x) crayon::bgBlue(crayon::white(crayon::bold(x))) else function(x) x
            title <- if(has_crayon) function(x) crayon::blue(crayon::bold(x)) else function(x) x
            subtitle <- if(has_crayon) function(x) crayon::green(x) else function(x) x
            highlight <- if(has_crayon) function(x) crayon::yellow(x) else function(x) x
            normal <- if(has_crayon) function(x) crayon::silver(x) else function(x) x

            num_geom_verts <- length(nodes(object@geometry))
            num_data_indices <- length(object@indices)

            cat("\n")
            cat(header(" NeuroSurface "), "\n\n")

            cat(title("  Geometry & Data Mapping:"), "\n")
            cat("  ", subtitle("Hemisphere:"), "         ", highlight(object@geometry@hemi), "\n", sep="")
            cat("  ", subtitle("Total Vertices:"), "   ", highlight(format(num_geom_verts, big.mark=",")), "\n", sep="")
            cat("  ", subtitle("Vertices w/ Data:"), "", highlight(format(num_data_indices, big.mark=",")), "\n", sep="")
            cat("\n")

            cat(title("  Data Summary:"), "\n")
            if (num_data_indices > 0) {
                data_summary <- summary(object@data[object@indices]) # Summarize only the valid data
                cat("  ", subtitle("Min:"), "    ", highlight(format(data_summary[["Min."]], digits=4)), "\n", sep="")
                cat("  ", subtitle("Median:"), "", highlight(format(data_summary[["Median"]], digits=4)), "\n", sep="")
                cat("  ", subtitle("Mean:"), "  ", highlight(format(data_summary[["Mean"]], digits=4)), "\n", sep="")
                cat("  ", subtitle("Max:"), "    ", highlight(format(data_summary[["Max."]], digits=4)), "\n", sep="")
            } else {
                cat("   ", normal("(No data values to summarize)"), "\n")
            }
            cat("\n")
          })





#' @rdname left-methods
#' @export
setMethod(f="left", signature=c(x="BilatNeuroSurfaceVector"),
          def=function(x) {
            x@left
          })



#' @rdname right-methods
#' @export
setMethod(f="right", signature=c(x="BilatNeuroSurfaceVector"),
          def=function(x) {
            x@right
          })



#' @keywords internal
normalize <- function(vals) {
  rng <- range(vals)
  denom <- diff(rng)
  if (denom == 0) {
    return(rep(0, length(vals)))
  }
  (vals - rng[1]) / denom
}




#' Extract Data from NeuroSurfaceVector
#'
#' @description
#' Extracts data from a NeuroSurfaceVector using bracket notation.
#'
#' @export
#' @param x the object
#' @param i first index
#' @return A numeric vector of data values for the specified column
setMethod(f="[[", signature=signature(x = "NeuroSurfaceVector", i = "numeric"),
          def=function (x, i) {
            x@data[,i]

          })



#' Subset NeuroSurfaceVector
#'
#' @description
#' Extracts a subset of data from a NeuroSurfaceVector.
#'
#' @export
#' @param x the object
#' @param i first index (rows/vertices)
#' @param j second index (columns/time points)
#' @param ... additional args
#' @param drop whether to drop dimensions
#' @return A numeric matrix or vector of the extracted data subset
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "numeric", j = "numeric", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[i,j]

          })

#' Subset NeuroSurfaceVector by Column
#'
#' @description
#' Extracts columns (time points) from a NeuroSurfaceVector.
#'
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
#' @return A numeric matrix or vector of extracted column data
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "missing", j = "numeric", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[,j]
          })



#' Subset NeuroSurfaceVector by Row
#'
#' @description
#' Extracts rows (vertices) from a NeuroSurfaceVector.
#'
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
#' @return A numeric matrix or vector of extracted row data
setMethod(f="[", signature=signature(x = "NeuroSurfaceVector", i = "numeric", j = "missing", drop="ANY"),
          def=function (x, i, j, ..., drop=TRUE) {
            x@data[i,]
          })

#' Extract All Data from NeuroSurfaceVector
#'
#' @description
#' Extracts all data from a NeuroSurfaceVector as a matrix.
#'
#' @export
#' @param x the object
#' @param i first index
#' @param j second index
#' @param ... additional args
#' @param drop dimension
#' @return A numeric matrix containing all data
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
#' @rdname as.matrix-methods
setMethod("as.matrix", signature(x = "BilatNeuroSurfaceVector"), function(x) as(x, "matrix"))
