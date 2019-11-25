
#' write_surf_data
#'
#' @param surf a class of type \code{NeuroSurface} or \code{NeuroSurfaceVector}
#' @param outstem the name of the output file, not including extension
#' @param hemi name of hemisphere ("lh" or "rh")
#' @export
write_surf_data <- function(surf, outstem, hemi="") {
  assert_that(inherits(surf, "NeuroSurface") || inherits(surf, "NeuroSurfaceVector"))

  nodes <- surf@indices - 1
  keep <- nodes(surf@geometry) %in% surf@indices

  marker <- if (hemi == "") {
    ""
  } else {
    paste0("_", hemi)
  }

  if (inherits(surf, "NeuroSurfaceVector")) {
    dat <- as.matrix(surf@data[keep,])
    out <- as.data.frame(cbind(nodes, dat))
    fname <- paste0(outstem, marker, ".1D.dset")
    utils::write.table(out, file=fname, row.names=FALSE, col.names=FALSE, quote=FALSE)
  } else {
    dat <- surf@data
    out <- as.data.frame(cbind(nodes, dat[keep]))
    fname <- paste0(outstem, marker, ".1D.dset")
    utils::write.table(out, file=fname, row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
}




#' @importFrom stringr str_trim
#' @importFrom stringr str_split
load_spec <- function(spec) {
  base_dir <- dirname(normalizePath(spec))

  lin <- readLines(spec)
  lin <- lapply(lin, stringr::str_trim)

  newsurf_lines <-  grep("NewSurface", lin)
  stdef_lines <- grep("StateDef", lin)

  states <- unlist(lapply(lin[stdef_lines], function(sdef) {
    str_trim(str_split(sdef, "=")[[1]][[2]])
  }))

  keyval <- lapply(newsurf_lines, function(ns) {
    print(ns)
    lnum <- ns
    vars <- list()
    keys <- list()
    count <- 1

    while (((lnum + count) < length(lin)) && lin[[lnum + count]] != "") {
      curline <- lin[[lnum + count]]
      if (curline == "NewSurface") {
        break
      }

      ret <- str_trim(str_split(lin[[lnum + count]], "=")[[1]])

      vars[[count]] <- ret[[2]]
      keys[[count]] <- ret[[1]]
      count <- count + 1
    }
    names(vars) <- unlist(keys)
    vars
  })

  meshdomain <- sapply(keyval, function(x) x$LocalDomainParent)
  meshdomain <- meshdomain[(meshdomain != "./SAME") & (meshdomain != "SAME")]
  meshdomain <- meshdomain[!sapply(meshdomain, is.null)]
  domain <- meshdomain[[1]]

  curvature <- sapply(keyval, function(x) x$LocalCurvatureParent)
  curvature <- curvature[(curvature != "./SAME") & (curvature != "SAME")]
  curvature <- curvature[!sapply(curvature, is.null)]
  curvature <- curvature[[1]]

  surfaces <- sapply(keyval, function(x) {
    if (!is.null(x[["SurfaceName"]])) {
      x[["SurfaceName"]]
    } else if (!is.null(x[["FreeSurferSurface"]])) {
      x[["FreeSurferSurface"]]
    } else {
      stop(paste("Missing SurfaceName: ", x))
    }
  })

  embedDim <- sapply(keyval, function(x) {
    if (!is.null(x[["EmbedDimension"]])) {
      x[["EmbedDimension"]]
    } else {
      3
    }
  })


}


#' read a surface data set
#'
#' load a surface from a surface geometry file with optional mapped surface data
#'
#' @param surface_name the name of the file containing the surface geometry.
#' @param surface_data_name the name of the file containing the values to be mapped to the surface (optional).
#' @param colind the columns/samples to load (optional), only if \code{surface_data_name} is not \code{NULL}
#' @param nodeind the subset of node indices to load
#' @return an instance of the class:
#'  \code{\linkS4class{SurfaceGeometry}}
#'  or \code{\linkS4class{NeuroSurface}}
#'  or \code{\linkS4class{NeuroSurfaceVector}}
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



  assert_that(is(leftGeometry, "SurfaceGeometry"))
  assert_that(is(rightGeometry, "SurfaceGeometry"))

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


#' read_surf_geometry
#'
#' load a supported surface geometry from file
#'
#' @param surface_name the name of the file containing the surface geometry.
#' @export
read_surf_geometry <- function(surface_name) {
  surf_source <- SurfaceGeometrySource(surface_name)
  load_data(surf_source)

}


#' SurfaceGeometrySource
#'
#' Constructor for SurfaceGeometrySource
#'
#' @param surface_name the name of the file containing the surface geometry.
#' @export
#' @rdname SurfaceGeometrySource-class
SurfaceGeometrySource <- function(surface_name) {
  stopifnot(is.character(surface_name))
  stopifnot(file.exists(surface_name))
  meta_info <- .readHeader(surface_name)
  new("SurfaceGeometrySource", meta_info=meta_info)
}

#' @rdname show
setMethod(f="show", signature=signature("SurfaceGeometry"),
          def=function(object) {
            cat("SurfaceGeometry \n")
            cat("  hemisphere: ", if (object@hemi == "lh") "left" else if (object@hemi == "rh") "right" else object@hemi, "\n")
            cat("  num vertices:", length(nodes(object)), "\n")
            cat("  num faces:", ncol(object@mesh$it), "\n")
          })


#' Constructor for NeuroSurfaceSource
#'
#' @param surface_geom the name of the file containing the surface geometry or a \code{SurfaceGeometry} instance
#' @param surface_data_name the name of the file containing the data values to be mapped to the surface.
#' @param colind the subset of column indices to load from surface data matrix (if provided)
#' @param nodeind the subset of node indices to load from surface data matrix (if provided)
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
#' @importMethodsFrom neuroim2 coords
#' @export
setMethod(f="coords", signature=c("igraph"),
          def=function(x) {
            cbind(igraph:::vertex_attr(x, "x"),
                  igraph:::vertex_attr(x, "y"),
                  igraph:::vertex_attr(x, "z"))
          })



#' @rdname coords
#' @export
setMethod(f="coords", signature=c("SurfaceGeometry"),
          def=function(x) {
            t(x@mesh$vb[1:3,])
          })



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

#' @export
#' @rdname vertices
setMethod(f="vertices", signature=c("SurfaceGeometry"),
          def=function(x, indices) {
            t(x@mesh$vb[1:3,indices, drop=FALSE])
          })


#' @export
#' @rdname nodes
setMethod(f="nodes", signature=c("SurfaceGeometry"),
          def=function(x) {
            seq(1, ncol(x@mesh$vb))
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
#' @param threshold the two-element threshold range to use to define connected components
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
            x@data[ret$size@data < size] <- 0
            x
          })


#' @export
#' @importMethodsFrom neuroim2 conn_comp
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
            Matrix::t(x@data[i,])
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




#' @export
#' @rdname graph
setMethod("graph", signature(x="SurfaceGeometry"),
          def=function(x) {
            x@graph
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


#' NeuroSurface
#'
#' construct a new NeuroSurface object
#' @param geometry a \code{SurfaceGeometry} instance
#' @param indices an integer vector specifying the valid surface nodes.
#' @param data a \code{vector} of data values.
#' @export
NeuroSurface <- function(geometry, indices, data) {
  new("NeuroSurface", geometry=geometry, indices=as.integer(indices),
      data=data)

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



#' load a NeuroSurfaceVector
#'
#' @importFrom Matrix Matrix
#' @importMethodsFrom neuroim2 load_data
#' @export
#' @param x the data source to load
#' @rdname load_data
setMethod(f="load_data", signature=c("NeuroSurfaceVectorSource"),
          def=function(x) {

            geometry <- x@geometry

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

            svec <- new("NeuroSurfaceVector", geometry=geometry,
                        indices=as.integer(valid_nodes), data=mat)

          })


#' @export
#' @rdname load_data
setMethod(f="load_data", signature=c("SurfaceGeometrySource"),
          def=function(x) {
            geometry <- load_data(x@meta_info)
          })



#' @export
#' @rdname load_data
setMethod(f="load_data", signature=c("NeuroSurfaceSource"),
          def=function(x) {
            geometry <- x@geometry
            reader <- data_reader(x@data_meta_info,0)
            nodes <- neuroim2::read_columns(reader,as.integer(0)) + 1

            keep <- nodes %in% x@nodeind
            nodes <- nodes[keep]

            vals <- neuroim2::read_columns(reader, as.integer(x@colind))[,1]
            nvert <- ncol(geometry@mesh$vb)

            avals <- numeric(nvert)
            avals[nodes] <- vals[keep]
            surf<- NeuroSurface(geometry=geometry, indices = nodes, data=avals)

          })



#' construct a graph from a set of vertices and nodes faces.
#'
#' @export
#' @param vertices N-by-3 matrix of vertices
#' @param nodes matrix of node faces, where each row is a set of three vertex indices.
#' @return an \code{igraph} instance representing th mesh connectivity.
#' @importFrom igraph set.vertex.attribute simplify get.edgelist
#' @importFrom igraph graph_from_edgelist get.edgelist set.vertex.attribute
#' @importFrom igraph simplify graph.adjacency
meshToGraph <- function(vertices, nodes) {
  edge1 <- as.matrix(nodes[,1:2 ])
  edge2 <- as.matrix(nodes[,2:3 ])
  edge3 <- as.matrix(nodes[,c(1,3) ])
  edges <- rbind(edge1,edge2,edge3) + 1

  gg1 <- igraph::simplify(igraph::graph_from_edgelist(edges, directed=FALSE))

  emat <- igraph::get.edgelist(gg1)
  v1 <- vertices[emat[,1],]
  v2 <- vertices[emat[,2],]

  ED <- sqrt(rowSums((v1 - v2)^2))

  gg1 <- igraph::set.vertex.attribute(gg1, "x", igraph::V(gg1), vertices[,1])
  gg1 <- igraph::set.vertex.attribute(gg1, "y", igraph::V(gg1), vertices[,2])
  gg1 <- igraph::set.vertex.attribute(gg1, "z", igraph::V(gg1), vertices[,3])

  gg1 <- igraph::set.edge.attribute(gg1, "dist", igraph::E(gg1), ED)
  gg1

}

####

## curvature:

## curv <- Rvcg::vcgCurve(mesh)
## normalize <- function(vals) (vals - min(vals))/(max(vals)-min(vals))
## vbmean <- normalize(curv$meanitmax)
## ccol = ifelse(curv$meanitmax > 0, "red", "green")
## make triangles: tri = misc3d::makeTriangles(t(vertices), t(nodes)+1, color=ccol)
## draw: drawScene.rgl(tri)
## for normal rgl using shade3d:
## ccol <- rep(ccol,each=3)
####


#' @export
#' @importFrom utils read.table
#' @rdname load_data
setMethod(f="load_data", signature=c("FreesurferSurfaceGeometryMetaInfo"),
          def=function(x) {
            loadFSSurface(x)
          })



#' load Freesurfer ascii surface
#'
#' @param meta_info instance of type \code{FreesurferSurfaceGeometryMetaInfo}
#' @details requires rgl library
#' @return a class of type \code{NeuroSurface}
#' @importFrom plyr rbind.fill.matrix
#' @importFrom readr read_table
#' @export
loadFSSurface <- function(meta_info) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (meta_info@file_descriptor@file_format == "Freesurfer_BINARY") {
    bdat <- readFreesurferBinaryGeometry(meta_info@data_file)
    graph <- meshToGraph(bdat$coords, bdat$faces)
    mesh <- rgl::tmesh3d(as.vector(t(bdat$coords)), as.vector(t(bdat$faces))+1, homogeneous=FALSE)
    new("SurfaceGeometry",  mesh=mesh, graph=graph, hemi=meta_info@hemi)

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
    #new("SurfaceGeometry", source=new("SurfaceGeometrySource", meta_info=meta_info), mesh=mesh, graph=graph)
    new("SurfaceGeometry",  mesh=mesh, graph=graph, hemi=meta_info@hemi)
  }
}





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



#' @rdname curvature
#' @export
setMethod(f="curvature", signature=c(x="SurfaceGeometry"),
          def=function(x) {
            curv <- Rvcg::vcgCurve(x@mesh)
            vbmean <- normalize(curv$meanvb)
            vbmean
          })



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






