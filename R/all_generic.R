#' @importMethodsFrom neuroim2 data_reader
NULL


if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))



#' Construct Neighborhood Graph from Surface Mesh
#'
#' @description
#' This generic function constructs a neighborhood graph from a surface mesh using edge weights.
#' It allows for flexible definition of neighborhoods based on edge radius and custom edge weights.
#'
#' @param x An object representing a surface mesh. The specific class depends on the method implementation.
#' @param radius Numeric. The edge radius defining the neighborhood extent.
#' @param edgeWeights Numeric vector. Custom weights for edges, used to define edge distances.
#' @param nodes Integer vector. The subset of nodes to use in graph construction. If NULL, all nodes are used.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object representing the constructed neighborhood graph. The specific class depends on the method implementation.
#'
#' @details
#' The neighborhood graph is constructed by considering edges within the specified radius
#' and applying the provided edge weights. This function is particularly useful in
#' neuroimaging analyses for defining local connectivity on brain surfaces.
#'
#' @examples
#' \dontrun{
#' # Assuming 'surface_mesh' is a pre-defined surface mesh object
#' graph <- neighbor_graph(surface_mesh, radius = 5, 
#'                         edgeWeights = runif(nrow(surface_mesh$vertices)),
#'                         nodes = 1:1000)
#' }
#'
#' @seealso \code{\link{graph}}, \code{\link{vertices}}
#'
#' @export
setGeneric(name = "neighbor_graph", 
           def = function(x, radius, edgeWeights, nodes, ...) 
             standardGeneric("neighbor_graph"))



#' Extract Vertices from a Surface Object
#'
#' @description
#' Extracts the vertices from a surface object, providing a standardized 
#' interface across different surface representations.
#'
#' @param x An object representing a surface.
#' @param ... Additional arguments passed to methods.
#'
#' @return A matrix or data structure containing vertex information.
#'
#' @examples
#' \dontrun{
#' vertex_data <- vertices(surface_obj)
#' num_vertices <- nrow(vertex_data)
#' }
#'
#' @seealso \code{\link{nodes}}
#'
#' @export
setGeneric(name = "vertices", def = function(x, ...) standardGeneric("vertices"))

#' Extract Surface Node Numbers
#'
#' @description
#' Retrieves the node numbers from a surface object.
#'
#' @param x An object representing a surface.
#'
#' @return A vector of node numbers.
#'
#' @seealso \code{\link{vertices}}
#'
#' @export
setGeneric(name = "nodes", def = function(x) standardGeneric("nodes"))

#' Extract Geometry from Surface Object
#'
#' @description
#' Extracts the geometric representation from a surface object.
#'
#' @param x A surface object
#' @return A geometry object representing the surface structure
#'
#' @seealso \code{\link{vertices}}, \code{\link{nodes}}
#'
#' @rdname geometry
#' @export
setGeneric(name="geometry", def=function(x) standardGeneric("geometry"))


#' extract \code{igraph} object
#'
#' @param x the object to extract the graph from
#' @param ... extra args
#' @export
setGeneric(name="graph", def=function(x, ...) standardGeneric("graph"))

#' Generic Function for Smoothing a Surface or Associated Data
#'
#' The \code{smooth} function is a generic function designed to apply smoothing operations to various surface objects. The specific behavior of the function depends on the class of the object passed as the \code{x} argument. It can be used to smooth the geometry of a surface, the data associated with a surface, or other related operations depending on the method implemented for the object's class.
#'
#' @param x The surface object to smooth. The specific class of \code{x} determines the type of smoothing applied.
#' @param ... Additional arguments passed to the specific method for smoothing the surface object.
#'
#' @details
#' The \code{smooth} function provides a common interface for smoothing operations on different types of surface objects. The actual smoothing process varies based on the class of the object provided:
#' \itemize{
#'   \item For \code{\linkS4class{SurfaceGeometry}} objects, the function smooths the surface geometry, modifying the shape of the mesh to reduce noise.
#'   \item For \code{\linkS4class{NeuroSurface}} objects, the function smooths the data values associated with each vertex, preserving the surface geometry but producing a smoother dataset.
#' }
#'
#' Users should refer to the specific method documentation for the class of object they are working with to understand the exact behavior and parameters.
#'
#' @examples
#' \dontrun{
#'   # Smooth a SurfaceGeometry object
#'   smoothed_geom <- smooth(surface_geom_obj, type="taubin", lambda=0.7, iteration=10)
#'
#'   # Smooth a NeuroSurface object's data
#'   smoothed_data_surface <- smooth(neuro_surf_obj, sigma=5)
#' }
#'
#' @seealso \code{\link{smooth,SurfaceGeometry-method}}, \code{\link{smooth,NeuroSurface-method}}
#' @rdname smooth
#' @export
setGeneric(name="smooth", def=function(x, ...) standardGeneric("smooth"))



#' Compute Graph Laplacian
#'
#' @param x Object to compute Laplacian from
#' @param normalized Logical; whether to normalize the Laplacian
#' @param weights Edge weights for weighted Laplacian matrix
#' @param ... Additional arguments
#' @export
setGeneric(name="laplacian", def=function(x, normalized, weights, ...) standardGeneric("laplacian"))


#' Get Adjacency Graph
#'
#' @param x Graph structure
#' @param attr Character; edge attribute for weights in igraph object. If absent, weights are 0 or 1.
#' @param ... Additional arguments
#' @export
setGeneric(name="adjacency", def=function(x, attr, ...) standardGeneric("adjacency"))



#' Compute Surface Curvature Vector
#'
#' @param x Object to compute curvature from
#' @param ... Additional arguments
#' @export
setGeneric(name="curvature", def=function(x, ...) standardGeneric("curvature"))


#' Get Left Hemisphere
#'
#' @param x Surface object
#' @return Left hemisphere of the surface
#' @export
setGeneric(name="left", def=function(x) standardGeneric("left"))



#' Get Right Hemisphere
#'
#' @param x Surface object
#' @return Right hemisphere of the surface
#' @export
setGeneric(name="right", def=function(x) standardGeneric("right"))

#' Apply Cluster-Extent Threshold to Surface Data
#'
#' @param x Object to threshold
#' @param threshold Numeric threshold range
#' @param size Minimum cluster size in nodes
#' @param ... Additional arguments
#' @seealso \code{\link{conn_comp}}
#' @export
setGeneric(name="cluster_threshold", def=function(x, threshold, size, ...) standardGeneric("cluster_threshold"))


#' Plot Surface as an HTMLWidget
#'
#' @param x Surface object to plot
#' @param width The width of the widget (optional)
#' @param height The height of the widget (optional)
#' @param ... Additional arguments passed to the plotting function
#' @return An HTMLWidget object
#' @export
setGeneric(name="plot_js", def=function(x, width = NULL, height = NULL, ...) standardGeneric("plot_js"))



# Generic function to read image meta info given a file and a \code{\linkS4class{FileFormat}} instance.
# @param x file format
# @param file_name file name contianing meta information
# @export
# @rdname read_meta_info-methods
#setGeneric(name="read_meta_info", def=function(x, file_name) standardGeneric("read_meta_info"))






#' Create a Surface Widget
#'
#' @description
#' This generic function creates a widget for visualizing surface data, 
#' allowing for different implementations based on the type of surface object.
#'
#' @param x An object representing a surface (e.g., SurfaceGeometry, NeuroSurface).
#' @param width The width of the widget (optional).
#' @param height The height of the widget (optional).
#' @param ... Additional arguments for customizing the widget appearance and behavior.
#'
#' @return An HTMLWidget object representing the surface visualization.
#'
#' @details
#' The surfwidget function creates an interactive widget for visualizing 
#' surface data, such as brain surfaces. The specific implementation depends 
#' on the class of the object provided, allowing for customized behavior 
#' for different types of surface representations.
#'
#' @seealso \code{\link{plot_js}}, \code{\link{SurfaceGeometry}}, \code{\link{NeuroSurface}}
#'
#' @export
setGeneric(name = "surfwidget", 
           def = function(x, width = NULL, height = NULL, ...) 
             standardGeneric("surfwidget"))


#' Find Boundaries Between Regions on a Surface
#'
#' @description
#' This generic function identifies boundaries between different regions or parcellations 
#' on a surface. The implementation depends on the class of the input object.
#'
#' @param x An object representing a surface with region information.
#' @param method A character string specifying the boundary detection method.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object containing boundary information. The specific structure depends 
#' on the method implementation.
#'
#' @details
#' This function provides a high-level interface for finding boundaries between 
#' different regions on a surface mesh. It typically returns coordinates and metadata 
#' describing the boundaries between regions.
#'
#' @seealso \code{\link{find_roi_boundaries}}
#'
#' @export
setGeneric(name = "findBoundaries", 
           def = function(x, method = "midpoint", ...) 
             standardGeneric("findBoundaries"))


