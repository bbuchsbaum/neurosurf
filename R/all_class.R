#' @importClassesFrom neuroim2 FileFormat
NULL

setOldClass("mesh3d")
setOldClass("igraph")



#' SurfaceGeometry
#'
#' a three-dimensional surface consisting of a set of triangle vertices
#'
#' @rdname SurfaceGeometry-class
#' @slot mesh the underlying \code{mesh3d} object
#' @slot graph underlying graph structure
#' @export
setClass("SurfaceGeometry",
         representation=representation(mesh="mesh3d", graph="igraph", hemi="character"))





#' SurfaceGeometryMetaInfo
#'
#' This class contains meta information for brain surface geometry
#'
#' @rdname SurfaceGeometryMetaInfo-class
#' @slot header_file name of the file containing meta information
#' @slot data_file name of the file containing data
#' @slot file_descriptor descriptor of image file format
#' @slot vertices the number of surface vertices
#' @slot faces the number of faces
#' @slot embed_dimension the dimensionality of the embedding
#' @slot label a label indicating the type of surface (e.g. white, pial, inflated, flat, spherical)
#' @slot hemi a label indicating the hemisphere, one of ('lh', 'rh', or 'unknown')
#' @importClassesFrom neuroim2 FileFormat
#' @export
setClass("SurfaceGeometryMetaInfo",
         representation=
           representation(
             header_file="character",
             data_file="character",
             file_descriptor="FileFormat",
             vertices="integer",
             faces="integer",
             label="character",
             hemi="character",
             embed_dimension="integer"))


#' FreeSurferSurfaceGeometryMetaInfo
#'
#' This class contains meta information for brain surface geometry
#'
#' @rdname FreeSurferSurfaceGeometryMetaInfo-class
#' @export
setClass("FreesurferSurfaceGeometryMetaInfo", contains=c("SurfaceGeometryMetaInfo"))

#' SurfaceDataMetaInfo
#'
#' This class contains meta information for surface-based data (the values that map to a surface geometry)
#'
#' @rdname SurfaceDataMetaInfo-class
#' @slot header_file name of the file containing meta information
#' @slot data_file name of the file containing data
#' @slot file_descriptor descriptor of image file format
#' @slot node_count the number of nodes for which surface data exists
#' @slot nels the number of data vectors (typically the number of columns in the surface data matrix; nels = 1 for a single surface data set)
#' @slot label a label indicating the type of surface (e.g. white, pial, inflated, flat, spherical)
#' @export
setClass("SurfaceDataMetaInfo",
         representation=
           representation(
             header_file="character",
             data_file="character",
             file_descriptor="FileFormat",
             node_count="integer",
             nels="integer",
             label="character"))

#' NIMLSurfaceDataMetaInfo
#'
#' This class contains meta information for surface-based data for the NIML data format
#'
#' @rdname NIMLSurfaceDataMetaInfo-class
#' @slot data the numeric data matrix of surface values (rows = nodes, columns=surface vectors)
#' @slot node_indices the indices of the nodes for mapping to associated surface geometry.
#' @export
setClass("NIMLSurfaceDataMetaInfo",
         representation=
           representation(
             data="matrix",
             node_indices="integer"),
         contains=c("SurfaceDataMetaInfo"))





#' SurfaceGeometrySource
#'
#' A class that is used to produce a \code{\linkS4class{SurfaceGeometry}} instance
#' @rdname SurfaceGeometrySource-class
#' @slot meta_info a \code{\linkS4class{SurfaceGeometryMetaInfo}} instance
#' @export
setClass("SurfaceGeometrySource",
         representation=representation(meta_info="SurfaceGeometryMetaInfo"))


#' NeuroSurfaceSource
#'
#' A class that is used to produce a \code{\linkS4class{NeuroSurface}} instance
#'
#' @rdname NeuroSurfaceSource-class
#' @slot geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @slot dataMetaInfo a \code{\linkS4class{SurfaceDataMetaInfo}} instance
#' @slot colind the column index of the surface map to be loaded.
#' @slot nodeind the node indices of the surface map to be loaded.
#' @export
setClass("NeuroSurfaceSource", representation=
           representation(geometry="SurfaceGeometry",
                          data_meta_info="SurfaceDataMetaInfo",
                          colind="integer",
                          nodeind="integer"))

#' NeuroSurfaceVectorSource
#'
#' A class that is used to produce a \code{\linkS4class{NeuroSurfaceVector}} instance
#'
#' @rdname NeuroSurfaceVectorSource-class
#' @slot geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @slot dataMetaInfo a \code{\linkS4class{SurfaceDataMetaInfo}} instance
#' @slot colind the column indices vector of the surface maps to be loaded
#' @export
setClass("NeuroSurfaceVectorSource", representation=
           representation(geometry="SurfaceGeometry",
                          data_meta_info="SurfaceDataMetaInfo",
                          colind="integer",
                          nodeind="integer"),
         contains=c("NeuroSurfaceSource"))







#' NIMLSurfaceFileDescriptor
#'
#' This class supports the NIML file format for surface-based data
#'
#' @rdname NIMLSurfaceFileDescriptor-class
#' @export
setClass("NIMLSurfaceFileDescriptor", contains=c("FileFormat"))


#' AFNISurfaceFileDescriptor
#'
#' This class supports the AFNI 1D file format for surface-based data
#' @rdname AFNISurfaceFileDescriptor-class
#' @export
setClass("AFNISurfaceFileDescriptor", contains=c("FileFormat"))


#' FresurferAsciiSurfaceFileDescriptor
#'
#' This class supports the Freesurfer Ascii file format for surface geometry
#' @rdname FreesurferAsciiSurfaceFileDescriptor-class
#' @export
setClass("FreesurferAsciiSurfaceFileDescriptor", contains=c("FileFormat"))



#' ROISurface
#'
#' A class that respresents a surface-based region of interest
#'
#' @slot geometry the geometry of the parent surface: a \code{SurfaceGeometry} instance
#' @slot data the vector-valued \code{numeric} data stored in ROI
#' @slot coords the surface-based coordinates of the data
#' @slot indices the node indices of the parent surface stored in the \code{geometry} field.
#' @importClassesFrom neuroim2 ROI
#' @exportClass ROISurface
#' @rdname ROISurface-class
setClass("ROISurface",
         representation=representation(geometry="SurfaceGeometry", data="numeric",
                                       coords="matrix", indices="integer"),
         validity = function(object) {
           if (ncol(object@coords) != 3) {
             stop("coords slot must be a matrix with 3 columns")
           }
           if (nrow(object@coords) != length(object@indices)) {
             stop("length of indices must equal nrow(coords)")
           }
         }, contains="ROI")

#' ROISurfaceVector
#'
#' A class that respresents a surface-based region of interest
#'
#' @slot geometry the geometry of the parent surface: a \code{SurfaceGeometry} instance
#' @slot data \code{matrix} data stored in ROI with number of columns equal to number of coordinates in ROI.
#' @slot coords the surface-based coordinates of the data
#' @slot indices the nodes of the parent surface stored in the \code{geometry} field.
#' @exportClass ROISurfaceVector
#' @rdname ROISurfaceVector-class
setClass("ROISurfaceVector",
         representation=representation(geometry="SurfaceGeometry", data="matrix",
                                       coords="matrix", indices="integer"),
         validity = function(object) {
           if (ncol(object@coords) != 3) {
             stop("coords slot must be a matrix with 3 columns")
           }
           if (nrow(object@coords) != length(object@indices)) {
             stop("length of indices must equal nrow(coords)")
           }

           if (ncol(object@data) != nrow(object@coords)) {
             stop("'ncol(data)' must equal 'nrow(coords)'")
           }
         }, contains="ROI")



#' NeuroSurface
#'
#' a three-dimensional surface consisting of a set of triangle vertices with one value per vertex.
#'
#' @rdname NeuroSurface-class
#' @slot geometry the surface geometry, an instance of \code{SurfaceGeometry}
#' @slot indices an \code{integer} vector specifying the subset of valid surface nodes encoded in the \code{geometry} object.
#' @slot data the 1-D vector of data value at each vertex of the mesh
#' @export
setClass("NeuroSurface",
         representation=representation(geometry="SurfaceGeometry",
                                       indices="integer",
                                       data="numeric"),
         validity = function(object) {
           if (length(data) != length(indices)) {
             stop("length of 'data' must equal length of 'indices'")
           }
         })

#' NeuroSurfaceVector
#'
#' a three-dimensional surface consisting of a set of triangle vertices with multiple values per vertex.
#'
#' @rdname NeuroSurfaceVector-class
#' @slot geometry the surface geometry, an instance of \code{SurfaceGeometry}
#' @slot indices an \code{integer} vector specifying the subset of valid surface nodes from in the \code{geometry} object.
#' @slot data a \code{Matrix} of values where each column contains a vector of values
#' over all the surface nodes. The number of rows of this \code{Matrix} must equal the number of nodes in the \code{geometry} object.
#' @export
setClass("NeuroSurfaceVector",
         representation=representation(geometry="SurfaceGeometry",
                                       indices="integer", data="Matrix"))


#' BilatNeuroSurfaceVector
#'
#' @slot left the surface data for left hemisphere, an instance of \code{NeuroSurfaceVector}
#' @slot right the surface data for right hemisphere, an instance of \code{NeuroSurfaceVector}
#' @export
setClass("BilatNeuroSurfaceVector",
  representation=representation(
                              left="NeuroSurfaceVector",
                              right="NeuroSurfaceVector"))







