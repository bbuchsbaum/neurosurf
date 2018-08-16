#' @include all_class.R
#' @include all_generic.R
NULL


.readHeader <- function(file_name) {
  desc <- findSurfaceDescriptor(file_name)
  if (is.null(desc)) {
    stop(paste("could not find reader for file: ", file_name))
  }

  read_meta_info(desc, file_name)
}


#' readFreesurferAsciiHeader
#'
#' @param file_name the file
#' @export
readFreesurferAsciiHeader <- function(file_name) {
  ninfo <- as.integer(strsplit(readLines(file_name, n=2)[2], " ")[[1]])
  list(vertices=ninfo[1], faces=ninfo[2], label=neuroim2:::strip_extension(FREESURFER_ASCII_SURFACE_DSET, basename(file_name)),
       embed_dimension=3, header_file=file_name, data_file=file_name)
}

#' readFreesurferAsciiGeometry
#'
#' @param file_name the file
#' @importFrom readr read_table
#' @export
readFreesurferAsciiGeometry <- function(file_name) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ninfo <- as.integer(strsplit(readLines(file_name, n=2)[2], " ")[[1]])
  asctab <- read_table(file_name, skip=2)
  #asctab <- readr::read_table(file_name, skip=2, col_names=FALSE)
  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])

  list(mesh=rgl::tmesh3d(vertices, nodes), header_file=file_name, data_file=file_name)

}


#' readAFNISurfaceHeader
#'
#' @param file_name the name of the AFNI 1D file
#' @importFrom readr read_table
#' @export
readAFNISurfaceHeader <- function(file_name) {

  #dmat <- readr::read_table(file_name, col_names=FALSE)
  dmat <- read.table(file_name, header=FALSE)

  list(header_file=file_name, data_file=file_name,
       node_count=nrow(dmat), nels=ncol(dmat)-1,
       label=neuroim2:::strip_extension(AFNI_SURFACE_DSET, basename(file_name)),
       data=as.matrix(dmat[,2:ncol(dmat)]), nodes=as.vector(dmat[,1]))

}


#' readNIMLSurfaceHeader
#
#' @param file_name the name of the NIML file
#' @export
readNIMLSurfaceHeader <- function(file_name) {
  p <- neuroim2:::parse_niml_file(file_name)
  whdat <- which(unlist(lapply(p, "[[", "label")) == "SPARSE_DATA")
  dmat <- if (length(whdat) > 1) {
    t(do.call(rbind, lapply(p[[whdat]], "[[", "data")))
  } else {
    t(p[[whdat]]$data)
  }

  whind <- which(unlist(lapply(p, "[[", "label")) == "INDEX_LIST")

  if (length(whind) == 0) {
    warning("readNIMLSurfaceHeader: assuming index is first column of data matrix")
    idat <- dmat[,1]
    dmat <- dmat[, 2:ncol(dmat)]
  } else {
    idat <- p[[whind]]$data[1,]
  }

  list(header_file=file_name, data_file=file_name,
       node_count=nrow(dmat), nels=ncol(dmat),
       label=neuroim2:::strip_extension(NIML_SURFACE_DSET, basename(file_name)),
       data=dmat, nodes=idat)
}



#' @rdname read_meta_info-methods
#' @importMethodsFrom neuroim2 read_meta_info
#' @export
setMethod(f="read_meta_info",signature=signature(x= "AFNISurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readAFNISurfaceHeader, AFNISurfaceDataMetaInfo)
          })

#' read_meta_info
#'
#' @rdname read_meta_info-methods
#' @export
setMethod(f="read_meta_info",signature=signature(x= "NIMLSurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readNIMLSurfaceHeader, NIMLSurfaceDataMetaInfo)
          })
#' read_meta_info
#'
#' @rdname read_meta_info-methods
#' @export
setMethod(f="read_meta_info",signature=signature(x= "FreesurferAsciiSurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readFreesurferAsciiHeader, FreesurferSurfaceGeometryMetaInfo)
          })


.read_meta_info <- function(desc, file_name, readFunc, constructor) {
  hfile <- neuroim2:::header_file(desc, file_name)
  header <- readFunc(hfile)
  header$file_name <- hfile
  constructor(desc, header)
}

#' return a reader function
#'
#' @rdname data_reader-methods
setMethod(f="data_reader", signature=signature("SurfaceGeometryMetaInfo"),
          def=function(x) {
            reader <- function(i) {
              if (length(i) == 1 && i == 0) {
                x@node_indices
              } else {
                x@data[,i,drop=FALSE]
              }
            }

            new("ColumnReader", nrow=as.integer(nrow(x@data)), ncol=as.integer(ncol(x@data)), reader=reader)
          })



#' @rdname data_reader-methods
setMethod(f="data_reader", signature=signature("NIMLSurfaceDataMetaInfo"),
          def=function(x) {
            reader <- function(i) {
              if (length(i) == 1 && i == 0) {
                x@node_indices
              } else {
                x@data[,i,drop=FALSE]
              }
            }

            new("ColumnReader", nrow=as.integer(nrow(x@data)), ncol=as.integer(ncol(x@data)), reader=reader)
          })




findSurfaceDescriptor <- function(file_name) {
  if (neuroim2:::file_matches(NIML_SURFACE_DSET, file_name)) NIML_SURFACE_DSET
  else if (neuroim2:::file_matches(FREESURFER_ASCII_SURFACE_DSET, file_name)) FREESURFER_ASCII_SURFACE_DSET
  else if (neuroim2:::file_matches(AFNI_SURFACE_DSET, file_name)) AFNI_SURFACE_DSET
  else NULL
}

NIML_SURFACE_DSET <- new("NIMLSurfaceFileDescriptor",
                         file_format="NIML",
                         header_encoding="raw",
                         header_extension="niml.dset",
                         data_encoding="raw",
                         data_extension="niml.dset")

AFNI_SURFACE_DSET <- new("AFNISurfaceFileDescriptor",
                         file_format="1D",
                         header_encoding="raw",
                         header_extension="1D.dset",
                         data_encoding="raw",
                         data_extension="1D.dset")

FREESURFER_ASCII_SURFACE_DSET <- new("FreesurferAsciiSurfaceFileDescriptor",
                                     file_format="Freesurfer_ASCII",
                                     header_encoding="text",
                                     header_extension="asc",
                                     data_encoding="raw",
                                     data_extension="asc")



#' Constructor for \code{\linkS4class{SurfaceGeometryMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
FreesurferSurfaceGeometryMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$vertices))
  stopifnot(is.numeric(header$faces))

  new("FreesurferSurfaceGeometryMetaInfo",
      header_file=header$header_file,
      data_file=header$data_file,
      file_descriptor=descriptor,
      vertices=as.integer(header$vertices),
      faces=as.integer(header$faces),
      label=as.character(header$label),
      embed_dimension=as.integer(header$embed_dimension))
}


#' Constructor for \code{\linkS4class{SurfaceDataMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
SurfaceDataMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$nodes))

  new("SurfaceDataMetaInfo",
      header_file=header$header_file,
      data_file=header$data_file,
      file_descriptor=descriptor,
      node_count=as.integer(header$nodes),
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
      header_file=header$header_file,
      data_file=header$data_file,
      file_descriptor=descriptor,
      node_count=as.integer(header$node_count),
      nels=as.integer(header$nels),
      label=as.character(header$label),
      data=header$data,
      node_indices=header$nodes)
}

#' Constructor for \code{\linkS4class{AFNISurfaceDataMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#'
AFNISurfaceDataMetaInfo <- function(descriptor, header) {
  stopifnot(is.numeric(header$nodes))

  new("NIMLSurfaceDataMetaInfo",
      header_file=header$header_file,
      data_file=header$data_file,
      file_descriptor=descriptor,
      node_count=as.integer(header$node_count),
      nels=as.integer(header$nels),
      label=as.character(header$label),
      data=header$data,
      node_indices=header$nodes)
}


#' show an \code{SurfaceGeometryMetaInfo}
#' @param object the object
setMethod(f="show", signature=signature("SurfaceGeometryMetaInfo"),
          def=function(object) {
            cat("an instance of class",  class(object), "\n\n")
            cat("number of vertices:", "\t", object@vertices, "\n")
            cat("number of faces:", "\t", object@faces, "\n")
            cat("label:", "\t", object@label, "\n")
            cat("embed dimension:", "\t", object@embed_dimension, "\n")
          })

#' show an \code{SurfaceDataMetaInfo}
#' @param object the object
setMethod(f="show", signature=signature("SurfaceDataMetaInfo"),
          def=function(object) {
            cat("an instance of class",  class(object), "\n\n")
            cat("node_count:", "\t", object@node_count, "\n")
            cat("nels:", "\t", object@nels, "\n")
            cat("label:", "\t", object@label, "\n")
          })

