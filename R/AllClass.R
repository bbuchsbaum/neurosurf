
setOldClass("mesh3d")
setOldClass("igraph")


#' SurfaceGeometry
#'
#' a three-dimensional surface consisting of a set of triangle vertices
#' @rdname SurfaceGeometry-class
#' @slot source the data source for the surface geometry
#' @slot mesh the underlying \code{mesh3d} object
#' @slot graph underlying graph structure
#' @export
setClass("SurfaceGeometry",
         representation=representation(source="BaseSource", mesh="mesh3d", graph="igraph"))


#' SurfaceGeometryMetaInfo
#'
#' This class contains meta information for brain surface geometry
#'
#' @rdname SurfaceGeometryMetaInfo-class
#' @slot headerFile name of the file containing meta information
#' @slot dataFile name of the file containing data
#' @slot fileDescriptor descriptor of image file format
#' @slot vertices the number of surface vertices
#' @slot faces the number of faces
#' @slot embedDimension the dimensionality of the embedding
#' @slot label a label indicating the type of surface (e.g. white, pial, inflated, flat, spherical)
#' @export
setClass("SurfaceGeometryMetaInfo",
         representation=
           representation(
             headerFile="character",
             dataFile="character",
             fileDescriptor="BrainFileDescriptor",
             vertices="integer",
             faces="integer",
             label="character",
             embedDimension="integer"),
         contains=c("BaseMetaInfo"))


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
#' @slot headerFile name of the file containing meta information
#' @slot dataFile name of the file containing data
#' @slot fileDescriptor descriptor of image file format
#' @slot nodeCount the number of nodes for which surface data exists
#' @slot nels the number of data vectors (typically the number of columns in the surface data matrix; nels = 1 for a single surface data set)
#' @slot label a label indicating the type of surface (e.g. white, pial, inflated, flat, spherical)
#' @export
setClass("SurfaceDataMetaInfo",
         representation=
           representation(
             headerFile="character",
             dataFile="character",
             fileDescriptor="BrainFileDescriptor",
             nodeCount="integer",
             nels="integer",
             label="character"),
         contains=c("BaseMetaInfo"))

#' NIMLSurfaceDataMetaInfo
#'
#' This class contains meta information for surface-based data for the NIML data format
#'
#' @rdname NIMLSurfaceDataMetaInfo-class
#' @slot data the numeric data matrix of surface values (rows = nodes, columns=surface vectors)
#' @slot nodeIndices the indices of the nodes for mapping to associated surface geometry.
#' @export
setClass("NIMLSurfaceDataMetaInfo",
         representation=
           representation(
             data="matrix",
             nodeIndices="integer"),
         contains=c("SurfaceDataMetaInfo"))





#' SurfaceGeometrySource
#'
#' A class that is used to produce a \code{\linkS4class{SurfaceGeometry}} instance
#' @rdname SurfaceGeometrySource-class
#' @slot metaInfo a \code{\linkS4class{SurfaceGeometryMetaInfo}} instance
#' @importClassesFrom neuroim BaseSource
#' @export
setClass("SurfaceGeometrySource",
         representation=representation(metaInfo="SurfaceGeometryMetaInfo"), contains="BaseSource")


#' BrainSurfaceSource
#'
#' A class that is used to produce a \code{\linkS4class{BrainSurface}} instance
#'
#' @rdname BrainSurfaceSource-class
#' @slot geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @slot dataMetaInfo a \code{\linkS4class{SurfaceDataMetaInfo}} instance
#' @slot colind the column index of the surface map to be loaded.
#' @slot nodeind the node indices of the surface map to be loaded.
#' @export
setClass("BrainSurfaceSource", representation=
           representation(geometry="SurfaceGeometry",
                          dataMetaInfo="SurfaceDataMetaInfo",
                          colind="integer",
                          nodeind="integer"),
         contains=c("BaseSource"))

#' BrainSurfaceVectorSource
#'
#' A class that is used to produce a \code{\linkS4class{BrainSurfaceVector}} instance
#'
#' @rdname BrainSurfaceVectorSource-class
#' @slot geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @slot dataMetaInfo a \code{\linkS4class{SurfaceDataMetaInfo}} instance
#' @slot colind the column indices vector of the surface maps to be loaded
#' @export
setClass("BrainSurfaceVectorSource", representation=
           representation(geometry="SurfaceGeometry",
                          dataMetaInfo="SurfaceDataMetaInfo",
                          colind="integer",
                          nodeind="integer"),
         contains=c("BrainSurfaceSource"))







#' NIMLSurfaceFileDescriptor
#'
#' This class supports the NIML file format for surface-based data
#'
#' @rdname NIMLSurfaceFileDescriptor-class
#' @importClassesFrom neuroim BrainFileDescriptor
#' @export
setClass("NIMLSurfaceFileDescriptor", contains=c("BrainFileDescriptor"))


#' AFNISurfaceFileDescriptor
#'
#' This class supports the AFNI 1D file format for surface-based data
#' @rdname AFNISurfaceFileDescriptor-class
#' @export
setClass("AFNISurfaceFileDescriptor", contains=c("BrainFileDescriptor"))


#' FresurferAsciiSurfaceFileDescriptor
#'
#' This class supports the Freesurfer Ascii file format for surface geometry
#' @rdname FreesurferAsciiSurfaceFileDescriptor-class
#' @export
setClass("FreesurferAsciiSurfaceFileDescriptor", contains=c("BrainFileDescriptor"))



#' ROISurface
#'
#' A class that respresents a surface-based region of interest
#'
#' @slot geometry the geometry of the parent surface: a \code{SurfaceGeometry} instance
#' @slot data the vector-valued \code{numeric} data stored in ROI
#' @slot coords the surface-based coordinates of the data
#' @slot indices the node indices of the parent surface stored in the \code{geometry} field.
#' @importClassesFrom neuroim ROI
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
         representation=representation(geometry="SurfaceGeometry", data="numericOrMatrix",
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



#' BrainSurface
#'
#' a three-dimensional surface consisting of a set of triangle vertices with one value per vertex.
#'
#' @rdname BrainSurface-class
#' @slot source the data source for the surface
#' @slot geometry the surface geometry, an instance of \code{SurfaceGeometry}
#' @slot indices an \code{integer} vector specifying the subset valid surface nodes encoded in the \code{geometry} object.
#' @slot data the 1-D vector of data value at each vertex of the mesh
#' @importClassesFrom neuroim BaseSource
#' @export
setClass("BrainSurface",
         representation=representation(source="BaseSource",
                                       geometry="SurfaceGeometry",
                                       indices="integer",
                                       data="numeric"))

#' BrainSurfaceVector
#'
#' a three-dimensional surface consisting of a set of triangle vertices with multiple values per vertex.
#'
#' @rdname BrainSurfaceVector-class
#' @slot source the data source for the surface
#' @slot geometry the surface geometry, an instance of \code{SurfaceGeometry}
#' @slot indices an \code{integer} vector specifying the subset valid surface nodes encoded in the \code{geometry} object.
#' @slot data a \code{Matrix} of values where each column contains a vector of values
#' over all the surface nodes.
#' @export
setClass("BrainSurfaceVector",
         representation=representation(source="BaseSource",
                                       geometry="SurfaceGeometry",
                                       indices="integer", data="Matrix"))


#' BilatBrainSurfaceVector
#'
#' @slot left the surface data for left hemisphere, an instance of \code{BrainSurfaceVector}
#' @slot right the surface data for right hemisphere, an instance of \code{BrainSurfaceVector}
#' @export
setClass("BilatBrainSurfaceVector",
  representation=representation(
                              left="BrainSurfaceVector",
                              right="BrainSurfaceVector"))



setClass("BaseColorPlane")

setClass("IntensityColorPlane",
         representation(intensity="numeric", alpha="numeric", colmap="character"), contains="BaseColorPlane")

setClass("ColorPlane", representation(clrs="ANY"), contains="BaseColorPlane")

setClass("HexColorPlane", representation(clrs="character"), contains="ColorPlane")

setClass("RGBColorPlane", representation(clrs="matrix"), contains="ColorPlane")

setClass("ConstantColorPlane", representation(clrs="character"), contains="ColorPlane")




