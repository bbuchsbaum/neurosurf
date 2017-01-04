#' readFreesurferAsciiHeader
#' @param fileName the file
#' @export
readFreesurferAsciiHeader <- function(fileName) {
  ninfo <- as.integer(strsplit(readLines(fileName, n=2)[2], " ")[[1]])
  list(vertices=ninfo[1], faces=ninfo[2], label=stripExtension(FREESURFER_ASCII_SURFACE_DSET, basename(fileName)),
       embedDimension=3, headerFile=fileName, dataFile=fileName)
}

#' readFreesurferAsciiGeometry
#' @param fileName the file
#' @export
readFreesurferAsciiGeometry <- function(fileName) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ninfo <- as.integer(strsplit(readLines(fileName, n=2)[2], " ")[[1]])
  #asctab <- read_table(fileName, skip=2)
  asctab <- read.table(fileName, skip=2)
  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])

  list(mesh=rgl::tmesh3d(vertices, nodes), headerFile=fileName, dataFile=fileName)

}


#' readAFNISurfaceHeader
#' @param fileName the name of the AFNI 1D file
#' @export
readAFNISurfaceHeader <- function(fileName) {
  dmat <- read.table(fileName)
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
  p <- parse_niml_file(fileName)
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



#' @rdname readMetaInfo-methods
#' @export
setMethod(f="readMetaInfo",signature=signature(x= "AFNISurfaceFileDescriptor"),
          def=function(x, fileName) {
            .readMetaInfo(x, fileName, readAFNISurfaceHeader, AFNISurfaceDataMetaInfo)
          })


#' @rdname readMetaInfo-methods
#' @export
setMethod(f="readMetaInfo",signature=signature(x= "NIMLSurfaceFileDescriptor"),
          def=function(x, fileName) {
            .readMetaInfo(x, fileName, readNIMLSurfaceHeader, NIMLSurfaceDataMetaInfo)
          })

#' @rdname readMetaInfo-methods
#' @export
setMethod(f="readMetaInfo",signature=signature(x= "FreesurferAsciiSurfaceFileDescriptor"),
          def=function(x, fileName) {
            .readMetaInfo(x, fileName, readFreesurferAsciiHeader, FreesurferSurfaceGeometryMetaInfo)
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


