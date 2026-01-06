#' @include all_class.R
#' @include all_generic.R
NULL

#' @noRd
#' @keywords internal
.readHeader <- function(file_name) {
  desc <- findSurfaceDescriptor(file_name)
  if (is.null(desc)) {
    stop(paste("could not find reader for file: ", file_name))
  }

  read_meta_info(desc, file_name)
}


#' Read Freesurfer Annotation File
#'
#' @description
#' Reads a Freesurfer annotation file and creates a LabeledNeuroSurface object.
#'
#' @param file_name Character string; path to the '.annot' file
#' @param geometry A SurfaceGeometry object representing the surface structure
#'
#' @return A LabeledNeuroSurface object containing:
#'   \item{indices}{Integer vector of vertex indices}
#'   \item{data}{Numeric vector of label codes}
#'   \item{labels}{Character vector of label names}
#'   \item{cols}{Character vector of label colors in hex format}
#'
#' @details
#' This function reads binary data from a FreeSurfer annotation file,
#' which includes vertex labels, color information, and label names.
#' It then constructs a LabeledNeuroSurface object using this information
#' along with the provided surface geometry.
#'
#'
#' @examples
#' \donttest{
#' # Read a FreeSurfer annotation file (requires actual annotation file)
#' geom <- example_surface_geometry()
#' # labeled_surface <- read_freesurfer_annot("lh.aparc.annot", geom)
#' }
#'
#' @export
read_freesurfer_annot <- function(file_name, geometry) {
  fp <- file(file_name, "rb")
  on.exit(close(fp))
  nvertex <- readBin(fp, integer(),n = 1, size=4, endian="big")
  vertex_dat <- readBin(fp, integer(),n = nvertex*2, size=4, endian="big")
  vertices <- vertex_dat[seq(1,length(vertex_dat), by=2)]
  clabs <- vertex_dat[seq(2,length(vertex_dat), by=2)]
  tags <- readBin(fp, integer(),n=4, size=4, endian="big")
  ## the third and fourth elements of `tags` contain the maximum
  ## structure index and the length of the following filename.  These
  ## values are not currently used, so we simply skip over the filename
  ## string.
  readChar(fp, tags[4], useBytes=TRUE)
  nlut <- readBin(fp, integer(),n=1, size=4, endian="big")
  labs <- vector(nlut, mode="list")
  for (i in 1:nlut) {
    lnum <- readBin(fp, integer(),n=1, size=4, endian="big")
    len <- readBin(fp, integer(),n=1, size=4, endian="big")
    label <- readChar(fp, len, useBytes=TRUE)
    rgba <- readBin(fp, integer(),n=4, size=4, endian="big")
    #(B * 256^2) + (G * 256) + (R)
    labs[[i]] <- list(
      num=lnum,
      label=label,
      red=rgba[1],
      green=rgba[2],
      blue=rgba[3],
      col=grDevices::rgb(rgba[1]/255, rgba[2]/255, rgba[3]/255),
      code=rgba[3] * 256^2 + (rgba[2] * 256) + rgba[1]
    )
  }

  codes <- match(clabs, sapply(labs, "[[", "code"))
  labels <- sapply(labs, "[[", "label")
  cols <- sapply(labs, "[[", "col")

  new("LabeledNeuroSurface", geometry=geometry,
      indices=as.integer(vertices+1),
      data=as.numeric(codes),
      labels=as.character(labels),
      cols=as.character(cols))

}

#' @noRd
#' @keywords internal
readGIFTIHeader <- function(file_name) {
  hdr <- gifti::readgii(file_name)
  list(header_file=file_name, data_file=file_name,
       info=hdr,
       label=neuroim2::strip_extension(GIFTI_SURFACE_DSET, basename(file_name)))
}

#' @noRd
#' @keywords internal
readGIFTIGZHeader <- function(file_name) {
  hdr <- gifti::readgii(file_name)
  list(header_file=file_name, data_file=file_name,
       info=hdr,
       label=neuroim2::strip_extension(GIFTI_GZ_SURFACE_DSET, basename(file_name)))
}


#' readFreesurferAsciiHeader
#'
#' @param file_name the file
#' @noRd
#' @keywords internal
readFreesurferAsciiHeader <- function(file_name) {
  has_hemi <- grep(".*\\.[lr]h\\..*", file_name)
  hemi <- if (length(has_hemi) > 0) {
    if (length(grep(".*\\.lh\\..*", file_name))>0) {
      "lh"
    } else if (length(grep(".*\\.rh\\..*", file_name)) > 0) {
      "rh"
    } else {
      "unknown"
    }
  } else {
    "unknown"
  }

  ninfo <- as.integer(strsplit(readLines(file_name, n=2)[2], " ")[[1]])
  list(vertices=ninfo[1], faces=ninfo[2], label=neuroim2::strip_extension(FREESURFER_ASCII_SURFACE_DSET, basename(file_name)),
       embed_dimension=3, header_file=file_name, data_file=file_name, hemi=hemi)
}

#' readFreesurferAsciiGeometry
#'
#' @param file_name the file
#' @importFrom readr read_table
#' @noRd
#' @keywords internal
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

  list(vertices=vertices, nodes=nodes, header_file=file_name, data_file=file_name)

}

#' readFreesurferBinaryHeader
#'
#' @param file_name the file
#' @noRd
#' @keywords internal
readFreesurferBinaryHeader <- function(file_name) {
  # Check for hemisphere in filename - look for _lh. or _rh. or starting with lh/rh
  base_name <- basename(file_name)
  hemi <- if (length(grep("(^|_)lh\\.", base_name)) > 0) {
    "lh"
  } else if (length(grep("(^|_)rh\\.", base_name)) > 0) {
    "rh"
  } else {
    "unknown"
  }

  fp <- file(file_name, "rb")
  on.exit(close(fp))
  
  # Check file size
  file_info <- file.info(file_name)
  if (file_info$size < 7) {
    stop("File too small to be a valid FreeSurfer binary header")
  }
  
  magic <- readBin(fp, what="raw", n=3)
  if (length(magic) < 3) {
    stop("Unable to read magic bytes from FreeSurfer file")
  }
  
  created_by <- readLines(fp, 2)
  vcount <- readBin(fp, what="integer", n=1, endian="big")
  fcount <- readBin(fp, what="integer", n=1, endian="big")
  
  if (length(vcount) == 0 || length(fcount) == 0) {
    stop("Unable to read vertex/face counts from FreeSurfer file")
  }

  list(vertices=vcount, faces=fcount, label=basename(file_name),
       embed_dimension=3, header_file=file_name, data_file=file_name, hemi=hemi)
}

#' readFreesurferBinaryGeometry
#'
#' @param file_name the file
#' @importFrom readr read_table
#' @noRd
#' @keywords internal
readFreesurferBinaryGeometry <- function(file_name) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }

  fp <- file(file_name, "rb")
  on.exit(close(fp))
  
  # Check file size
  file_info <- file.info(file_name)
  if (file_info$size < 7) {
    stop("File too small to be a valid FreeSurfer binary geometry")
  }
  
  magic <- readBin(fp, what="raw", n=3)
  if (length(magic) < 3) {
    stop("Unable to read magic bytes from FreeSurfer file")
  }
  
  created_by <- readLines(fp, 2)
  vcount <- readBin(fp, what="integer", n=1, endian="big")
  fcount <- readBin(fp, what="integer", n=1, endian="big")
  
  if (length(vcount) == 0 || length(fcount) == 0) {
    stop("Unable to read vertex/face counts from FreeSurfer file")
  }

  coords <- readBin(fp, what="double", n=vcount*3, size=4, endian="big")
  if (length(coords) < vcount*3) {
    stop("Unable to read all vertex coordinates from FreeSurfer file")
  }
  coords <- matrix(coords, vcount, 3, byrow=TRUE)

  faces <- readBin(fp, what="integer", n=fcount*3, size=4, endian="big")
  if (length(faces) < fcount*3) {
    stop("Unable to read all face indices from FreeSurfer file")
  }
  faces <- matrix(faces, fcount, 3, byrow=TRUE)

  list(coords=coords, faces=faces, header_file=file_name, data_file=file_name)

}



#' readAFNISurfaceHeader
#'
#' @param file_name the name of the AFNI 1D file
#' @importFrom readr read_table
#' @noRd
#' @keywords internal
readAFNISurfaceHeader <- function(file_name) {

  #dmat <- readr::read_table(file_name, col_names=FALSE)
  dmat <- read.table(file_name, header=FALSE)

  list(header_file=file_name, data_file=file_name,
       node_count=nrow(dmat), nels=ncol(dmat)-1,
       label=neuroim2::strip_extension(AFNI_SURFACE_DSET, basename(file_name)),
       data=as.matrix(dmat[,2:ncol(dmat)]), nodes=as.vector(dmat[,1]))

}


#' Parse a NIML element string into label and attributes
#'
#' @param el Character string containing NIML element
#' @return List with label and attr components
#' @noRd
#' @keywords internal
.parse_niml_element <- function(el) {
  items <- strsplit(el, " ")[[1]]
  if (length(items) > 1) {
    items <- items[items != "" & items != ">"]
    label <- items[1]
    # Parse key=value pairs
    attr_list <- list()
    for (it in items[-1]) {
      keyval <- strsplit(it, "=")[[1]]
      if (length(keyval) == 2) {
        attr_list[[keyval[1]]] <- keyval[2]
      }
    }
    list(label = label, attr = attr_list)
  } else if (length(items) == 1) {
    list(label = items[[1]], attr = NULL)
  } else {
    list(label = NULL, attr = NULL)
  }
}

#' Parse a NIML header from file connection
#'
#' @param fconn File connection opened in binary mode
#' @return List with label and attr components
#' @noRd
#' @keywords internal
.parse_niml_header <- function(fconn) {
  out <- c()
  STATE <- "BEGIN"
  while (TRUE) {
    ch <- suppressWarnings(readChar(fconn, 1))
    if (length(ch) == 0) {
      break
    } else if (ch == "<" && STATE == "BEGIN") {
      STATE <- "HEADER"
    } else if (ch == ">" && STATE == "HEADER") {
      STATE <- "END"
      break
    } else {
      out <- c(out, ch)
    }
  }
  out <- paste(out, collapse = "")
  out <- gsub("\n", "", out)
  out <- gsub("\"", "", out)
  out <- gsub("/", "", out)
  .parse_niml_element(trimws(out))
}

#' Read binary NIML data
#'
#' @param fconn File connection
#' @param meta List with ni_type, ni_dimen, ni_form attributes
#' @return Matrix of data values
#' @noRd
#' @keywords internal
.read_niml_data <- function(fconn, meta) {
  dtype <- meta$ni_type
  dtype <- strsplit(as.character(dtype), "\\*")[[1]]
  if (length(dtype) == 2) {
    nvols <- as.integer(dtype[1])
    type <- dtype[2]
  } else {
    nvols <- 1
    type <- dtype[1]
  }
  type <- switch(type,
                 int = "integer",
                 double = "double",
                 float = "double")
  nels <- as.integer(meta$ni_dimen)

  if (!is.null(meta$ni_form) && meta$ni_form == "binary.lsbfirst") {
    allvals <- readBin(fconn, what = type, size = 4, n = nels * nvols)
    mat <- matrix(allvals, nvols, nels)
  } else {
    ret <- readLines(fconn, n = nels * nvols + 1)[-1]
    if (type == "integer") {
      matrix(as.integer(trimws(ret)), nvols, nels)
    } else if (type == "double") {
      matrix(as.numeric(trimws(ret)), nvols, nels)
    } else {
      stop(paste("unrecognized type: ", type))
    }
  }
}

#' Parse next NIML element from file connection
#'
#' @param fconn File connection opened in binary mode
#' @return List with label, attr, and optionally data components
#' @noRd
#' @keywords internal
.parse_niml_next <- function(fconn) {
  header <- .parse_niml_header(fconn)
  if (!is.null(header$attr) &&
      (header$label == "SPARSE_DATA" || header$label == "INDEX_LIST")) {
    header$data <- .read_niml_data(fconn, header$attr)
  }
  # Skip to closing tag
  STATE <- "BEGIN"
  while (TRUE) {
    ch <- suppressWarnings(readChar(fconn, 1))
    if (length(ch) == 0) {
      break
    } else if (ch == "<" && STATE == "BEGIN") {
      STATE <- "CLOSE_TAG"
    } else if (ch == ">" && STATE == "CLOSE_TAG") {
      break
    }
  }
  header
}

#' Parse a NIML file
#'
#' @param fname File path to NIML file
#' @param maxels Maximum number of elements to parse
#' @return List of parsed elements
#' @noRd
#' @keywords internal
.parse_niml_file <- function(fname, maxels = 10000) {
  fconn <- file(fname, open = "rb")
  on.exit(close(fconn))
  fsize <- file.info(fname)$size
  out <- list()
  elcount <- 1
  out[[elcount]] <- .parse_niml_header(fconn)
  while (seek(fconn, where = NA) < fsize && elcount < maxels) {
    elcount <- elcount + 1
    el <- .parse_niml_next(fconn)
    out[[elcount]] <- el
  }
  out
}

#' readNIMLSurfaceHeader
#'
#' @param file_name the name of the NIML file
#' @noRd
#' @keywords internal
readNIMLSurfaceHeader <- function(file_name) {
  # Use our fixed NIML parser instead of neuroim2's buggy one
  p <- .parse_niml_file(file_name)
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
       label=neuroim2::strip_extension(NIML_SURFACE_DSET, basename(file_name)),
       data=dmat, nodes=idat)
}



#' Write Surface Data to File
#'
#' This function writes surface data from a NeuroSurface or NeuroSurfaceVector object to a .1D.dset file.
#'
#' @param surf An object of class \code{NeuroSurface} or \code{NeuroSurfaceVector} containing the surface data to be written.
#' @param outstem A character string specifying the base name for the output file (without extension).
#' @param hemi A character string specifying the hemisphere ("lh" for left, "rh" for right). Default is an empty string.
#'
#' @details
#' The function writes the surface data to a .1D.dset file, which is a tabular data format.
#' The output file contains node indices in the first column, followed by data values in subsequent columns.
#' The file name is constructed by combining \code{outstem}, \code{hemi} (if provided), and the extension ".1D.dset".
#' For NeuroSurfaceVector objects, all columns of data are written. For NeuroSurface objects, only the single data vector is written.
#' The data is written without row names, column names, or quotes.
#'
#' @return
#' This function does not return a value. It writes the data to a .1D.dset file as a side effect.
#'
#' @examples
#' \donttest{
#' # Assuming 'surf_data' is a NeuroSurface or NeuroSurfaceVector object
#' write_surf_data(surf_data, "output_data", "lh")
#' # This will create a file named "output_data_lh.1D.dset"
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom utils write.table
#' @export
write_surf_data <- function(surf, outstem, hemi="") {
  assert_that(inherits(surf, "NeuroSurface") || inherits(surf, "NeuroSurfaceVector"))

  nodes <- surf@indices - 1

  marker <- if (hemi == "") {
    ""
  } else {
    paste0("_", hemi)
  }

  if (inherits(surf, "NeuroSurfaceVector")) {
    dat <- as.matrix(surf@data)
    out <- as.data.frame(cbind(nodes, dat))
    fname <- paste0(outstem, marker, ".1D.dset")
    utils::write.table(out, file=fname, row.names=FALSE, col.names=FALSE, quote=FALSE)
  } else {
    dat <- surf@data
    out <- as.data.frame(cbind(nodes, dat))
    fname <- paste0(outstem, marker, ".1D.dset")
    utils::write.table(out, file=fname, row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
}


#' Read Surface Data from a File
#'
#' @description
#' This function reads surface data from a file in one of the supported formats.
#'
#' @param surface_name the name of the file containing the surface geometry.
#' @param surface_data_name the name of the file containing the values to be mapped to the surface (optional).
#' @param colind the columns/samples to load (optional), only if \code{surface_data_name} is not \code{NULL}
#' @param nodeind the subset of node indices to load
#'
#' @return an instance of the class:
#'  \code{\linkS4class{SurfaceGeometry}}
#'  or \code{\linkS4class{NeuroSurface}}
#'  or \code{\linkS4class{NeuroSurfaceVector}}
#'
#' @details
#' The function supports reading surface data from various formats including:
#' \itemize{
#'   \item Freesurfer ASCII (.asc)
#'   \item Freesurfer binary
#'   \item GIFTI (.gii)
#'   \item NIML Surface Dataset (.niml.dset)
#' }
#'
#' The format is determined automatically from the file extension.
#'
#' @examples
#' \donttest{
#' # Find the path to the example surface file in the package
#' surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
#'
#' # Check if the file exists
#' if (file.exists(surf_file)) {
#'   # Read the surface geometry (returns SurfaceGeometry for geometry-only files)
#'   surf <- read_surf(surf_file)
#'
#'   # Display basic information about the surface
#'   print(surf)
#'
#'   # Get vertex coordinates
#'   head(coords(surf))
#' }
#' }
#' @export
read_surf  <- function(surface_name, surface_data_name=NULL, colind=NULL, nodeind=NULL) {
  if (is.null(surface_data_name)) {
    surf_source <- SurfaceGeometrySource(surface_name)
    load_data(surf_source)
  } else {
    src <- NeuroSurfaceSource(surface_name, surface_data_name, colind, nodeind)
    load_data(src)
  }
}

#' load surface data and link to \code{\linkS4class{SurfaceGeometry}}
#'
#' @param geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @param surface_data_name the name of the file containing the values to be mapped to the surface.
#' @param colind the subset column indices of surface dataset to load (optional)
#' @param nodeind the subset node indices of surface dataset to include (optional)
#' @return an instance of the class \code{\linkS4class{NeuroSurface}} or \code{\linkS4class{NeuroSurfaceVector}}
#' @export
read_surf_data  <- function(geometry, surface_data_name, colind=NULL, nodeind=NULL) {
  src <- NeuroSurfaceSource(geometry, surface_data_name, colind, nodeind)
  load_data(src)
}


#' read_surf_data_seq
#'
#' load one or more surface datasets for both left and right hemispheres
#'
#' @param leftGeometry a \code{\linkS4class{SurfaceGeometry}} instance for the left hemisphere
#' @param rightGeometry a \code{\linkS4class{SurfaceGeometry}} instance for the right hemisphere
#' @param leftDataNames a \code{character} vector indicating names of left-hemisphere surface data files to be mapped to geometry.
#' @param rightDataNames a \code{character} vector indicating names of right-hemisphere surface data files to be mapped to geometry.
#' @importFrom assertthat assert_that
#' @export
read_surf_data_seq <- function(leftGeometry, rightGeometry, leftDataNames, rightDataNames) {
  assert_that(length(leftDataNames) == length(rightDataNames))

  if (is.character(leftGeometry)) {
    leftGeometry <- read_surf_geometry(leftGeometry)
  }

  if (is.character(rightGeometry)) {
    rightGeometry <- read_surf_geometry(rightGeometry)
  }

  assert_that(is_surface_like(leftGeometry))
  assert_that(is_surface_like(rightGeometry))

  ret <- lapply(1:length(leftDataNames), function(i) {
    src1 <- NeuroSurfaceSource(leftGeometry, leftDataNames[i], NULL)
    src2 <- NeuroSurfaceSource(rightGeometry, rightDataNames[i], NULL)
    list(left=load_data(src1), right=load_data(src2))
  })

  lind <- ret[[1]]$left@indices
  rind <- ret[[1]]$right@indices

  ldat <- do.call(cbind, lapply(ret, function(x) x$left@data))
  rdat <- do.call(cbind, lapply(ret, function(x) x$right@data))

  left <- new("NeuroSurfaceVector", geometry=leftGeometry, indices=lind,data=ldat)
  right <- new("NeuroSurfaceVector", geometry=rightGeometry, indices=rind,data=rdat)

  ret <- new("BilatNeuroSurfaceVector", left=left, right=right)

}


#' read_meta_info
#'
#' @param x the file descriptor object
#' @param file_name the name of the file containing meta information.
#' @export
#' @rdname read_meta_info-methods
#' @importMethodsFrom neuroim2 read_meta_info
setMethod(f="read_meta_info",signature=signature(x= "AFNISurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readAFNISurfaceHeader, NIMLSurfaceDataMetaInfo)
          })

#' @rdname read_meta_info-methods
#' @export
setMethod(f="read_meta_info",signature=signature(x= "NIMLSurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readNIMLSurfaceHeader, NIMLSurfaceDataMetaInfo)
          })


#' @rdname read_meta_info-methods
#' @export
setMethod(f="read_meta_info",signature=signature(x= "FreesurferAsciiSurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readFreesurferAsciiHeader, FreesurferSurfaceGeometryMetaInfo)
          })


#' @rdname read_meta_info-methods
#' @export
setMethod(f="read_meta_info",signature=signature(x= "FreesurferBinarySurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readFreesurferBinaryHeader, FreesurferSurfaceGeometryMetaInfo)
          })

#' @rdname read_meta_info-methods
#' @export
setMethod(f="read_meta_info",signature=signature(x= "GIFTISurfaceFileDescriptor"),
          def=function(x, file_name) {
            .read_meta_info(x, file_name, readGIFTIHeader, GIFTISurfaceGeometryMetaInfo)
          })



#' load_data
#' @rdname load_data-methods
#' @export
setMethod(f="load_data", signature=c("NeuroSurfaceVectorSource"),
          def=function(x) {

            geom_source <- x@geometry
            geometry <- resolve_surface_geometry(geom_source)

            reader <- data_reader(x@data_meta_info,0)

            ## the node indices of the data file -- this could be a subset of the nodes in the surface geometry.
            nodes <- neuroim2::read_columns(reader, as.integer(0)) + 1

            mat <- neuroim2::read_columns(reader, as.integer(x@colind))
            nvert <- ncol(geometry@mesh$vb)

            ## check for all-zero columns
            allzero <- apply(mat, 1, function(vals) all(vals == 0))

            ## the set of valid nodes
            keep <- (nodes %in% x@nodeind) & !allzero
            valid_nodes <- nodes[keep]

            mat <- if (nvert > length(valid_nodes) && length(valid_nodes)/nvert < .5) {
              ## sparse matrix
              M <- do.call(rbind, lapply(1:ncol(mat), function(i) {
                cbind(i=valid_nodes, j=i, x=mat[keep,i])
              }))

              Matrix::sparseMatrix(i=M[,1], j=M[,2], x=M[,3], dims=c(length(nodes), ncol(mat)))
            } else if (nvert > length(valid_nodes)) {
              ## dense
              m <- matrix(0, nvert, ncol(mat))
              m[valid_nodes, 1:ncol(mat)] <- mat[keep,]
              Matrix::Matrix(m)
            } else {
              Matrix::Matrix(mat)
            }

            svec <- new("NeuroSurfaceVector", geometry=geom_source,
                        indices=as.integer(valid_nodes), data=mat)

          })




#' @export
#' @rdname load_data-methods
setMethod(f="load_data", signature=c("NeuroSurfaceSource"),
          def=function(x) {
            geometry <- x@geometry
            reader <- data_reader(x@data_meta_info,as.integer(0))
            nodes <- neuroim2::read_columns(reader,as.integer(0)) + 1

            keep <- nodes %in% x@nodeind
            nodes <- nodes[keep]

            vals <- neuroim2::read_columns(reader, as.integer(x@colind))[,1]
            # nvert <- ncol(geometry@mesh$vb) # No longer needed for this approach

            # avals <- numeric(nvert) # No longer needed for this approach
            # avals[nodes] <- vals[keep] # No longer needed for this approach

            # Pass only the read indices and their corresponding data values
            surf <- NeuroSurface(geometry=geometry, indices = nodes, data = vals[keep])
          })




#' @export
#' @importFrom utils read.table
#' @rdname load_data-methods
setMethod(f="load_data", signature=c("FreesurferSurfaceGeometryMetaInfo"),
          def=function(x) {
            loadFSSurface(x)
          })

#' @export
#' @rdname load_data-methods
setMethod(f="load_data", signature=c("GIFTISurfaceGeometryMetaInfo"),
          def=function(x) {
            loadGIFTISurface(x)
          })

#' Load GIFTI surface geometry
#'
#' @param meta_info instance of type \code{GIFTISurfaceGeometryMetaInfo}
#' @details requires rgl library
#' @return a class of type \code{SurfaceGeometry}
#' @export
loadGIFTISurface <- function(meta_info) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # The info slot contains the gifti object from readgii()
  gii <- meta_info@info

  # Extract vertices (pointset) and faces (triangle) from gifti data
  vertices <- gii$data$pointset
  faces <- gii$data$triangle

  # Build graph from mesh
  graph <- meshToGraph(vertices, faces)

  # Create mesh - note that GIFTI faces are 0-indexed, need to add 1
  mesh <- rgl::tmesh3d(as.vector(t(vertices)), as.vector(t(faces)) + 1, homogeneous = FALSE)

  # Extract surf_to_world transform from GIFTI CoordinateSystemTransformMatrix if present
  # The transform is typically stored in data_info for the pointset data array
  surf_to_world <- .extract_gifti_transform(gii)

  new("SurfaceGeometry", mesh = mesh, graph = graph, hemi = meta_info@hemi,
      label = meta_info@label, surf_to_world = surf_to_world)
}


#' Extract coordinate transform from GIFTI object
#'
#' @param gii gifti object returned by gifti::readgii()
#' @return 4x4 affine transformation matrix (identity if not found)
#' @noRd
#' @keywords internal
.extract_gifti_transform <- function(gii) {
  # Default to identity
  xform <- diag(4)

  # The gifti package stores transform in data_info$trans_mat for each data array

  # We look for the pointset (NIFTI_INTENT_POINTSET) transform first
  if (!is.null(gii$data_info)) {
    for (i in seq_along(gii$data_info)) {
      info <- gii$data_info[[i]]
      # Check if this is the pointset and has a transform
      if (!is.null(info$trans_mat) && !is.null(info$intent)) {
        if (grepl("POINTSET|pointset", info$intent, ignore.case = TRUE)) {
          # trans_mat should be a 4x4 matrix
          if (is.matrix(info$trans_mat) &&
              nrow(info$trans_mat) == 4 && ncol(info$trans_mat) == 4) {
            xform <- info$trans_mat
            break
          }
        }
      }
    }

    # If no pointset-specific transform, check first available transform
    if (identical(xform, diag(4))) {
      for (i in seq_along(gii$data_info)) {
        info <- gii$data_info[[i]]
        if (!is.null(info$trans_mat)) {
          if (is.matrix(info$trans_mat) &&
              nrow(info$trans_mat) == 4 && ncol(info$trans_mat) == 4) {
            xform <- info$trans_mat
            break
          }
        }
      }
    }
  }

  xform
}




#' load Freesurfer ascii surface
#'
#' @param meta_info instance of type \code{FreesurferSurfaceGeometryMetaInfo}
#' @param surf_to_world Optional 4x4 affine transformation matrix from surface (tkr-RAS)
#'   coordinates to world (scanner RAS) coordinates. Defaults to identity. For proper
#'   alignment with volumes, this transform can be computed from FreeSurfer's vox2ras-tkr
#'   matrix (available in orig.mgz or via mri_info --vox2ras-tkr).
#' @details requires rgl library
#' @return a class of type \code{SurfaceGeometry}
#' @importFrom plyr rbind.fill.matrix
#' @importFrom readr read_table
#' @export
loadFSSurface <- function(meta_info, surf_to_world = diag(4)) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!is.matrix(surf_to_world) || nrow(surf_to_world) != 4 || ncol(surf_to_world) != 4) {
    stop("surf_to_world must be a 4x4 matrix.")
  }

  if (meta_info@file_descriptor@file_format == "Freesurfer_BINARY") {
    bdat <- readFreesurferBinaryGeometry(meta_info@data_file)
    graph <- meshToGraph(bdat$coords, bdat$faces)
    mesh <- rgl::tmesh3d(as.vector(t(bdat$coords)), as.vector(t(bdat$faces))+1, homogeneous=FALSE)
    new("SurfaceGeometry", mesh = mesh, graph = graph, hemi = meta_info@hemi,
        label = meta_info@label, surf_to_world = surf_to_world)

  } else {

    meshname <- meta_info@header_file
    ninfo <- as.integer(strsplit(readLines(meshname, n=2)[2], " ")[[1]])
    message("loading ", meshname)
    #asctab <- readr::read_table(meshname, skip=2, col_names=FALSE)
    asctab <- read.table(meshname, skip=2)

    vertices <- as.matrix(asctab[1:ninfo[1],1:3])
    nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])

    graph <- meshToGraph(vertices, nodes)

    if (meta_info@hemi == "unknown") {
      if (mean(vertices[,1]) < 0) {
        meta_info@hemi <- "lh"
      } else if (mean(vertices[,1]) > 0) {
        meta_info@hemi <- "rh"
      }
    }

    mesh <- rgl::tmesh3d(as.vector(t(vertices)), as.vector(t(nodes))+1, homogeneous=FALSE)
    new("SurfaceGeometry", mesh = mesh, graph = graph, hemi = meta_info@hemi,
        label = meta_info@label, surf_to_world = surf_to_world)
  }
}




#' @noRd
#' @keywords internal
.read_meta_info <- function(desc, file_name, readFunc, constructor) {
  hfile <- neuroim2::header_file(desc, file_name)
  header <- readFunc(hfile)
  header$file_name <- hfile
  constructor(desc, header)
}


#' data_reader
#' @param x A SurfaceGeometryMetaInfo object
#' @rdname data_reader-methods
#' @export
#' @importClassesFrom neuroim2 ColumnReader
setMethod(f="data_reader", signature=signature("SurfaceGeometryMetaInfo"),
          def=function(x) {
            if (!all(c("data", "node_indices") %in% slotNames(x))) {
              stop("data_reader requires 'data' and 'node_indices' slots",
                   call. = FALSE)
            }
            reader <- function(i) {
              if (length(i) == 1 && i == 0) {
                x@node_indices
              } else {
                x@data[, i, drop = FALSE]
              }
            }

            neuroim2::ColumnReader(nrow = as.integer(nrow(x@data)),
                                   ncol = as.integer(ncol(x@data)),
                                   reader = reader)
          })


#' @rdname data_reader-methods
#' @export
setMethod(f="data_reader", signature=signature("NIMLSurfaceDataMetaInfo"),
          def=function(x) {
            reader <- function(i) {
              if (length(i) == 1 && i == 0) {
                x@node_indices
              } else {
                x@data[,i,drop=FALSE]
              }
            }

            neuroim2::ColumnReader(nrow=as.integer(nrow(x@data)), ncol=as.integer(ncol(x@data)), reader=reader)
            #new("ColumnReader", nrow=as.integer(nrow(x@data)), ncol=as.integer(ncol(x@data)), reader=reader)
          })



#' @noRd
findSurfaceDescriptor <- function(file_name) {
  if (neuroim2::file_matches(NIML_SURFACE_DSET, file_name)) NIML_SURFACE_DSET
  else if (neuroim2::file_matches(FREESURFER_ASCII_SURFACE_DSET, file_name)) FREESURFER_ASCII_SURFACE_DSET
  else if (neuroim2::file_matches(AFNI_SURFACE_DSET, file_name)) AFNI_SURFACE_DSET
  else if (neuroim2::file_matches(GIFTI_SURFACE_DSET, file_name)) GIFTI_SURFACE_DSET
  else if (neuroim2::file_matches(GIFTI_GZ_SURFACE_DSET, file_name)) GIFTI_GZ_SURFACE_DSET
  else FREESURFER_BINARY_SURFACE_DSET
}

#' @noRd
GIFTI_SURFACE_DSET <- new("GIFTISurfaceFileDescriptor",
                         file_format="GIFTI",
                         header_encoding="raw",
                         header_extension="gii",
                         data_encoding="gii",
                         data_extension="gii")
#' @noRd
GIFTI_GZ_SURFACE_DSET <- new("GIFTISurfaceFileDescriptor",
                          file_format="GIFTI",
                          header_encoding="raw",
                          header_extension="gii.gz",
                          data_encoding="gii.gz",
                          data_extension="gii.gz")

#' @noRd
NIML_SURFACE_DSET <- new("NIMLSurfaceFileDescriptor",
                         file_format="NIML",
                         header_encoding="raw",
                         header_extension="niml.dset",
                         data_encoding="raw",
                         data_extension="niml.dset")

#' @noRd
AFNI_SURFACE_DSET <- new("AFNISurfaceFileDescriptor",
                         file_format="1D",
                         header_encoding="raw",
                         header_extension="1D.dset",
                         data_encoding="raw",
                         data_extension="1D.dset")

#' @noRd
FREESURFER_ASCII_SURFACE_DSET <- new("FreesurferAsciiSurfaceFileDescriptor",
                                     file_format="Freesurfer_ASCII",
                                     header_encoding="text",
                                     header_extension="asc",
                                     data_encoding="raw",
                                     data_extension="asc")

#' @noRd
FREESURFER_BINARY_SURFACE_DSET <- new("FreesurferBinarySurfaceFileDescriptor",
                                     file_format="Freesurfer_BINARY",
                                     header_encoding="raw",
                                     header_extension=".",
                                     #header_extension=c("orig", "pial", "inflated", "sphere", "sphere.reg", "white", "smoothwm", "thickness", "volume"),
                                     data_encoding="raw",
                                     data_extension=".")
                                     #data_extension=c("orig", "pial", "inflated", "sphere", "sphere.reg", "white", "smoothwm", "thickness", "volume"))



#' Constructor for \code{\linkS4class{SurfaceGeometryMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#' @noRd
#' @keywords internal
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
      hemi=header$hemi,
      embed_dimension=as.integer(header$embed_dimension))
}


#' Constructor for \code{\linkS4class{SurfaceDataMetaInfo}} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#' @noRd
#' @keywords internal
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
#' @noRd
#' @keywords internal
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
      node_indices=as.integer(header$nodes))
}

#' Create a \code{NIMLSurfaceDataMetaInfo} instance from an AFNI header
#'
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#' @noRd
#' @keywords internal
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
      node_indices=as.integer(header$nodes))
}

#' Constructor for \code{GIFTISurfaceGeometryMetaInfo} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#' @noRd
#' @keywords internal
GIFTISurfaceGeometryMetaInfo <- function(descriptor, header) {
  id0 <- which(header$info$data_info$name == "pointset")
  id1 <- which(header$info$data_info$name == "triangle")
  assertthat::assert_that(length(id0) > 0, msg="gifti surface file must have pointset")
  assertthat::assert_that(length(id1) > 0, msg="gifti surface file must have triangles")

  # Determine hemisphere from file name
  hemi <- "unknown"
  if (grepl("\\.lh\\.|_lh[._]|^lh\\.", basename(header$header_file))) {
    hemi <- "lh"
  } else if (grepl("\\.rh\\.|_rh[._]|^rh\\.", basename(header$header_file))) {
    hemi <- "rh"
  }

  new("GIFTISurfaceGeometryMetaInfo",
      header_file=header$header_file,
      data_file=header$data_file,
      file_descriptor=descriptor,
      vertices=as.integer(header$info$data_info$Dim0[id0]),
      faces=as.integer(header$info$data_info$Dim0[id1]),
      embed_dimension=3L,
      label=as.character(header$label),
      hemi=hemi,
      info=header$info)
}

#' Constructor for \code{GIFTISurfaceDataMetaInfo} class
#' @param descriptor the file descriptor
#' @param header a \code{list} containing header information
#' @noRd
#' @keywords internal
GIFTISurfaceDataMetaInfo <- function(descriptor, header) {
  #stopifnot(is.numeric(header$nodes))
  #browser()
  id0 <- which(header$info$data_info$name == "pointset")
  id1 <- which(header$info$data_info$name == "triangle")
  assertthat::assert_that(length(id0) > 0, msg="gifti surface file must have pointset")
  assertthat::assert_that(length(id1) > 0, msg="gifti surface file must have triangles")
  new("GIFTISurfaceDataMetaInfo",
      header_file=header$header_file,
      data_file=header$data_file,
      file_descriptor=descriptor,
      node_count=as.integer(header$info$data_info$Dim0[id0]),
      nels=1L,
      label=as.character(header$label),
      info=header$info)
}

#' show
#' @param object A SurfaceGeometryMetaInfo object
#' @rdname show-methods
#' @export
setMethod(f="show", signature=signature("SurfaceGeometryMetaInfo"),
          def=function(object) {
            cat("an instance of class",  class(object), "\n\n")
            cat("number of vertices:", "\t", object@vertices, "\n")
            cat("number of faces:", "\t", object@faces, "\n")
            cat("label:", "\t", object@label, "\n")
            cat("hemisphere:", "\t", object@hemi, "\n")
            cat("embed dimension:", "\t", object@embed_dimension, "\n")
          })

#' @rdname show-methods
#' @export
setMethod(f="show", signature=signature("SurfaceDataMetaInfo"),
          def=function(object) {
            cat("an instance of class",  class(object), "\n\n")
            cat("node_count:", "\t", object@node_count, "\n")
            cat("nels:", "\t", object@nels, "\n")
            cat("label:", "\t", object@label, "\n")
          })
