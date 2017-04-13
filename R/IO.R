


.readHeader <- function(fileName) {
  desc <- findDescriptor(fileName)
  if (is.null(desc)) {
    stop(paste("could not find reader for file: ", fileName))
  }

  readMetaInfo(desc, fileName)
}


#' readFreesurferAsciiHeader
#'
#' @param fileName the file
#' @export
readFreesurferAsciiHeader <- function(fileName) {
  ninfo <- as.integer(strsplit(readLines(fileName, n=2)[2], " ")[[1]])
  list(vertices=ninfo[1], faces=ninfo[2], label=stripExtension(FREESURFER_ASCII_SURFACE_DSET, basename(fileName)),
       embedDimension=3, headerFile=fileName, dataFile=fileName)
}

#' readFreesurferAsciiGeometry
#'
#' @param fileName the file
#' @importFrom readr read_table
#' @export
readFreesurferAsciiGeometry <- function(fileName) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ninfo <- as.integer(strsplit(readLines(fileName, n=2)[2], " ")[[1]])
  asctab <- read_table(fileName, skip=2)
  #asctab <- readr::read_table(fileName, skip=2, col_names=FALSE)
  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])

  list(mesh=rgl::tmesh3d(vertices, nodes), headerFile=fileName, dataFile=fileName)

}


#' readAFNISurfaceHeader
#'
#' @param fileName the name of the AFNI 1D file
#' @importFrom readr read_table
#' @export
readAFNISurfaceHeader <- function(fileName) {

  #dmat <- readr::read_table(fileName, col_names=FALSE)
  dmat <- read.table(fileName, header=FALSE)

  list(headerFile=fileName, dataFile=fileName,
       nodeCount=nrow(dmat), nels=ncol(dmat)-1,
       label=stripExtension(AFNI_SURFACE_DSET, basename(fileName)),
       data=as.matrix(dmat[,2:ncol(dmat)]), nodes=as.vector(dmat[,1]))

}


#' readNIMLSurfaceHeader
#
#' @param fileName the name of the NIML file
#' @export
readNIMLSurfaceHeader <- function(fileName) {
  p <- neuroim:::parse_niml_file(fileName)
  whdat <- which(unlist(lapply(p, "[[", "label")) == "SPARSE_DATA")
  dmat <- if (length(whdat) > 1) {
    t(do.call(rbind, lapply(p[[whdat]], "[[", "data")))
  } else {
    t(p[[whdat]]$data)
  }

  whind <- which(unlist(lapply(p, "[[", "label")) == "INDEX_LIST")
  idat <- p[[whind]]$data[1,]
  list(headerFile=fileName, dataFile=fileName,
       nodeCount=nrow(dmat), nels=ncol(dmat),
       label=stripExtension(NIML_SURFACE_DSET, basename(fileName)),
       data=dmat, nodes=idat)
}


#' readMetaInfo
#'
#' @rdname readMetaInfo-methods
#' @export
setMethod(f="readMetaInfo",signature=signature(x= "AFNISurfaceFileDescriptor"),
          def=function(x, fileName) {
            .readMetaInfo(x, fileName, readAFNISurfaceHeader, AFNISurfaceDataMetaInfo)
          })

#' readMetaInfo
#'
#' @rdname readMetaInfo-methods
#' @export
setMethod(f="readMetaInfo",signature=signature(x= "NIMLSurfaceFileDescriptor"),
          def=function(x, fileName) {
            .readMetaInfo(x, fileName, readNIMLSurfaceHeader, NIMLSurfaceDataMetaInfo)
          })
#' readMetaInfo
#'
#' @rdname readMetaInfo-methods
#' @export
setMethod(f="readMetaInfo",signature=signature(x= "FreesurferAsciiSurfaceFileDescriptor"),
          def=function(x, fileName) {
            .readMetaInfo(x, fileName, readFreesurferAsciiHeader, FreesurferSurfaceGeometryMetaInfo)
          })


.readMetaInfo <- function(desc, fileName, readFunc, constructor) {

  hfile <- headerFile(desc, fileName)
  header <- readFunc(hfile)
  header$fileName <- hfile
  constructor(desc, header)
}

#' dataReader
#'
#' @rdname dataReader-methods
setMethod(f="dataReader", signature=signature("SurfaceGeometryMetaInfo"),
          def=function(x) {
            reader <- function(i) {
              if (length(i) == 1 && i == 0) {
                x@nodeIndices
              } else {
                x@data[,i,drop=FALSE]
              }
            }

            new("ColumnReader", nrow=as.integer(nrow(x@data)), ncol=as.integer(ncol(x@data)), reader=reader)
          })


#' dataReader
#'
#' @rdname dataReader-methods
setMethod(f="dataReader", signature=signature("NIMLSurfaceDataMetaInfo"),
          def=function(x) {
            reader <- function(i) {
              if (length(i) == 1 && i == 0) {
                x@nodeIndices
              } else {
                x@data[,i,drop=FALSE]
              }
            }

            new("ColumnReader", nrow=as.integer(nrow(x@data)), ncol=as.integer(ncol(x@data)), reader=reader)
          })




findDescriptor <- function(fileName) {

  if (fileMatches(NIML_SURFACE_DSET, fileName)) NIML_SURFACE_DSET
  else if (fileMatches(FREESURFER_ASCII_SURFACE_DSET, fileName)) FREESURFER_ASCII_SURFACE_DSET
  else if (fileMatches(AFNI_SURFACE_DSET, fileName)) AFNI_SURFACE_DSET
  else NULL
}

NIML_SURFACE_DSET <- new("NIMLSurfaceFileDescriptor",
                         fileFormat="NIML",
                         headerEncoding="raw",
                         headerExtension="niml.dset",
                         dataEncoding="raw",
                         dataExtension="niml.dset")

AFNI_SURFACE_DSET <- new("AFNISurfaceFileDescriptor",
                         fileFormat="1D",
                         headerEncoding="raw",
                         headerExtension="1D.dset",
                         dataEncoding="raw",
                         dataExtension="1D.dset")

FREESURFER_ASCII_SURFACE_DSET <- new("FreesurferAsciiSurfaceFileDescriptor",
                                     fileFormat="Freesurfer_ASCII",
                                     headerEncoding="text",
                                     headerExtension="asc",
                                     dataEncoding="raw",
                                     dataExtension="asc")



#' Constructor for \code{\linkS4class{SurfaceGeometryMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
FreesurferSurfaceGeometryMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$vertices))
  stopifnot(is.numeric(header$faces))


  new("FreesurferSurfaceGeometryMetaInfo",
      headerFile=header$headerFile,
      dataFile=header$dataFile,
      fileDescriptor=descriptor,
      vertices=as.integer(header$vertices),
      faces=as.integer(header$faces),
      label=as.character(header$label),
      embedDimension=as.integer(header$embedDimension))
}



#' Constructor for \code{\linkS4class{SurfaceDataMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
SurfaceDataMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$nodes))

  new("SurfaceDataMetaInfo",
      headerFile=header$headerFile,
      dataFile=header$dataFile,
      fileDescriptor=descriptor,
      nodeCount=as.integer(header$nodes),
      nels=as.integer(header$nels),
      label=as.character(header$label))
}

#' Constructor for \code{\linkS4class{NIMLSurfaceDataMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#'
NIMLSurfaceDataMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$nodes))

  new("NIMLSurfaceDataMetaInfo",
      headerFile=header$headerFile,
      dataFile=header$dataFile,
      fileDescriptor=descriptor,
      nodeCount=as.integer(header$nodeCount),
      nels=as.integer(header$nels),
      label=as.character(header$label),
      data=header$data,
      nodeIndices=header$nodes)
}

#' Constructor for \code{\linkS4class{AFNISurfaceDataMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#'
AFNISurfaceDataMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$nodes))

  new("NIMLSurfaceDataMetaInfo",
      headerFile=header$headerFile,
      dataFile=header$dataFile,
      fileDescriptor=descriptor,
      nodeCount=as.integer(header$nodeCount),
      nels=as.integer(header$nels),
      label=as.character(header$label),
      data=header$data,
      nodeIndices=header$nodes)
}


#' show an \code{SurfaceGeometryMetaInfo}
#' @param object the object
#' @export
setMethod(f="show", signature=signature("SurfaceGeometryMetaInfo"),
          def=function(object) {
            cat("an instance of class",  class(object), "\n\n")
            cat("number of vertices:", "\t", object@vertices, "\n")
            cat("number of faces:", "\t", object@faces, "\n")
            cat("label:", "\t", object@label, "\n")
            cat("embed dimension:", "\t", object@embedDimension, "\n")
          })

#' show an \code{SurfaceDataMetaInfo}
#' @param object the object
#' @export
setMethod(f="show", signature=signature("SurfaceDataMetaInfo"),
          def=function(object) {
            cat("an instance of class",  class(object), "\n\n")
            cat("nodeCount:", "\t", object@nodeCount, "\n")
            cat("nels:", "\t", object@nels, "\n")
            cat("label:", "\t", object@label, "\n")
          })

