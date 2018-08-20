
#' write_surf_data
#'
#' @param bsurf a class of type \code{NeuroSurface} or \code{NeuroSurfaceVector}
#' @param outstem the name of the output file, not including extension
#' @param hemi name of hemisphere ("lh" or "rh")
#' @export
write_surf_data <- function(bsurf, outstem, hemi="") {
  assert_that(inherits(bsurf, "NeuroSurface") || inherits(bsurf, "NeuroSurfaceVector"))

  nodes <- bsurf@indices - 1
  keep <- nodes(bsurf@geometry) %in% bsurf@indices

  marker <- if (hemi == "") {
    ""
  } else {
    paste0("_", hemi)
  }

  if (inherits(bsurf, "NeuroSurfaceVector")) {
    dat <- as.matrix(bsurf@data[keep,])
    out <- as.data.frame(cbind(nodes, dat))
    fname <- paste0(outstem, marker, ".1D.dset")
    write.table(out, file=fname, row.names=FALSE, col.names=FALSE, quote=FALSE)
  } else {
    dat <- bsurf@data
    out <- as.data.frame(cbind(nodes, dat[keep]))
    fname <- paste0(outstem, marker, ".1D.dset")
    write.table(out, file=fname, row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
}




#' @importFrom stringr str_trim
#' @importFrom stringr str_split
loadSpec <- function(spec) {
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
  meshdomain <- meshdomain[!sapply(x, is.null)]
  domain <- meshdomain[[1]]

  curvature <- sapply(keyval, function(x) x$LocalCurvatureParent)
  curvature <- curvature[(curvature != "./SAME") & (curvature != "SAME")]
  curvature <- curvature[!sapply(x, is.null)]
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

#' load surface data and attach to \code{\linkS4class{SurfaceGeometry}}
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
#' load surface geometry from file
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

#' show a \code{SurfaceGeometry}
#' @param object the object
setMethod(f="show", signature=signature("SurfaceGeometry"),
          def=function(object) {
            cat("SurfaceGeometry \n")
            #cat("  file: ", object@source@meta_info@header_file, "\n")
            cat("  num vertices:", length(nodes(object)), "\n")
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

#' coords
#'
#' @rdname coords-methods
#' @importMethodsFrom neuroim2 coords
#' @export
setMethod(f="coords", signature=c("igraph"),
          def=function(x) {
            cbind(igraph:::vertex_attr(x, "x"),
                  igraph:::vertex_attr(x, "y"),
                  igraph:::vertex_attr(x, "z"))
          })



#' @rdname coords-methods
#' @export
setMethod(f="coords", signature=c("SurfaceGeometry"),
          def=function(x) {
            t(x@mesh$vb[1:3,])
          })



#' @rdname coords-methods
#' @export
setMethod(f="coords", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            coords(geometry(x))
          })


#' coords
#'
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

#' @rdname as.matrix-methods
#' @export
setMethod(f="as.matrix", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            as.matrix(x@data)
          })


#' @rdname vertices-methods
#' @export
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

#' @rdname vertices-methods
#' @export
setMethod(f="vertices", signature=c("SurfaceGeometry"),
          def=function(x, indices) {
            t(x@mesh$vb[1:3,indices, drop=FALSE])
          })

#' @rdname nodes-methods
#' @export
setMethod(f="nodes", signature=c("SurfaceGeometry"),
          def=function(x) {
            seq(1, ncol(x@mesh$vb))
          })

#' @rdname indices-methods
#' @importMethodsFrom neuroim2 indices
#' @export
setMethod(f="indices", signature=c("NeuroSurfaceVector"),
          def=function(x) {
            x@indices
          })

#' @rdname indices-methods
#' @importMethodsFrom neuroim2 indices
#' @export
setMethod(f="indices", signature=c("NeuroSurface"),
          def=function(x) {
            x@indices
          })

#' @rdname nodes-methods
#' @export
setMethod(f="nodes", signature=c("NeuroSurface"),
          def=function(x) {
            callGeneric(x@geometry)
          })

#' @rdname nodes-methods
#' @export
setMethod(f="nodes", signature=c(x="NeuroSurfaceVector"),
          def=function(x) {
            callGeneric(x@geometry)
          })


#' @rdname conn_comp-methods
#' @export
#' @importMethodsFrom neuroim2 conn_comp
setMethod(f="conn_comp", signature=c(x="NeuroSurface"),
          def=function(x, threshold) {
            keep <- x@data < threshold[1] | x@data > threshold[2]
            vs <- V(ssurf@geometry@graph)[keep]
            sg <- igraph::induced_subgraph(x@geometry@graph, vs)
            comps <- components(sg)



            memb <- numeric(length(keep))
            memb[keep] <- comps$membership

            sizes <- numeric(length(keep))
            sizes[keep] <- comps$csize[comps$membership]

            index_surf <- NeuroSurface(x@geometry, x@indices, memb)
            size_surf <- NeuroSurface(x@geometry, x@indices, sizes)

            list(index=index_surf, size=size_surf)
          })


#' series
#'
#' @rdname series-methods
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
#' @rdname series-methods
#' @return a class of type \code{ROISurfaceVector}
#' @importMethodsFrom neuroim2 series_roi
#' @export
setMethod("series_roi", signature(x="NeuroSurfaceVector", i="numeric"),
          def=function(x, i) {
            m <- as.matrix(series(x,i))
            ROISurfaceVector(geometry=x@geometry, indices=i, data=m)
          })

#' series
#'
#' @rdname series-methods
#' @importFrom Matrix Matrix
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="integer"),
          def=function(x, i) {
            Matrix::t(x@data[i,])
          })

#' series
#'
#' @rdname series-methods
#' @export
setMethod("series", signature(x="NeuroSurfaceVector", i="ROISurface"),
          def=function(x, i) {
            callGeneric(x, indices(i))
          })


#' series_roi
#'
#' @rdname series-methods
#' @importMethodsFrom neuroim2 series_roi
#' @export
setMethod("series_roi", signature(x="NeuroSurfaceVector", i="ROISurface"),
          def=function(x, i) {
            mat <- series(x, indices(i))
            ROISurfaceVector(x@geometry, indices(i), as.matrix(mat))
          })

#' series
#'
#' @rdname series-methods
#' @export
setMethod("series", signature(x="NeuroSurface", i="numeric"),
          def=function(x, i) {
            Matrix::t(x@data[i])
          })


#' map data using a lookup table
#'
#' @importMethodsFrom neuroim2 map_values
#' @export
#' @rdname map_values-methods
setMethod("map_values", signature(x="NeuroSurface", lookup="list"),
          def=function(x,lookup) {
            outv <- lookup[as.vector(x@data)]
            NeuroSurface(geometry=x@geometry, indices=x@indices, data=outv)

          })






#' @importMethodsFrom neuroim2 map_values
#' @export
#' @rdname map_values-methods
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



#' @rdname graph-methods
#' @export
setMethod("graph", signature(x="NeuroSurface"),
          def=function(x,...) {
            callGeneric(x@geometry)
          })



#' @rdname graph-methods
#' @export
setMethod("graph", signature(x="NeuroSurfaceVector"),
          def=function(x, ...) {
            callGeneric(x@geometry)
          })



#' @rdname graph-methods
#' @export
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

#' show a \code{NeuroSurfaceVector}
#' @param x the object
#' @param ... extra arguments
#' @export
setMethod(f="show", signature=signature("NeuroSurfaceVector"),
          def=function(object) {
            cat("NeuroSurfaceVector \n")
            cat("  num vertices: ", length(nodes(object@geometry)), "\n")
            cat("  num nonzero indices:", length(object@indices), "\n")
            cat("  num columns:", ncol(object@data), "\n")

          })

#' show a \code{NeuroSurface}
#' @param x the object
#' @param ... extra arguments
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
#' @rdname load_data-methods
setMethod(f="load_data", signature=c("NeuroSurfaceVectorSource"),
          def=function(x) {

            geometry <- x@geometry

            reader <- data_reader(x@data_meta_info,0)

            ## the node indices of the data file -- this could be a subset of the nodes in the surface geometry.
            nodes <- read_columns(reader, 0) + 1

            mat <- read_columns(reader, x@colind)
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

            svec <- new("NeuroSurfaceVector", source=x, geometry=geometry,
                        indices=as.integer(valid_nodes), data=mat)

          })

#' load a \code{SurfaceGeometry} instance
#'
#' @export
#' @rdname load_data-methods
setMethod(f="load_data", signature=c("SurfaceGeometrySource"),
          def=function(x) {
            geometry <- load_data(x@meta_info)
          })


#' load a NeuroSurface
#'
#' @export
#' @rdname load_data-methods
setMethod(f="load_data", signature=c("NeuroSurfaceSource"),
          def=function(x) {
            geometry <- x@geometry
            reader <- data_reader(x@data_meta_info,0)
            nodes <- read_columns(reader,0) + 1

            keep <- nodes %in% x@nodeind
            nodes <- nodes[keep]

            vals <- read_columns(reader, x@colind)[,1]
            nvert <- ncol(geometry@mesh$vb)

            avals <- numeric(nvert)
            avals[nodes] <- vals[keep]
            surf<- NeuroSurface(geometry=geometry, indices = nodes, data=avals, source=x)

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

#' load a Freesurfer surface geometry
#' @export
#' @importFrom utils read.table
#' @rdname load_data-methods
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

  meshname <- meta_info@header_file
  ninfo <- as.integer(strsplit(readLines(meshname, n=2)[2], " ")[[1]])
  message("loading ", meshname)
  #asctab <- readr::read_table(meshname, skip=2, col_names=FALSE)
  asctab <- read.table(meshname, skip=2)

  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])

  graph <- meshToGraph(vertices, nodes)

  mesh <- rgl::tmesh3d(as.vector(t(vertices)), as.vector(t(nodes))+1, homogeneous=FALSE)
  #new("SurfaceGeometry", source=new("SurfaceGeometrySource", meta_info=meta_info), mesh=mesh, graph=graph)
  new("SurfaceGeometry",  mesh=mesh, graph=graph)
}




#' left
#'
#' @rdname left-methods
#' @export
setMethod(f="left", signature=c(x="BilatNeuroSurfaceVector"),
          def=function(x) {
            x@left
          })


#' right
#'
#' @rdname left-methods
#' @export
setMethod(f="right", signature=c(x="BilatNeuroSurfaceVector"),
          def=function(x) {
            x@right
          })




normalize <- function(vals) (vals - min(vals))/(max(vals)-min(vals))


#' curvature
#'
#' @rdname curvature-methods
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

setMethod("as.matrix", signature(x = "BilatNeuroSurfaceVector"), function(x) as(x, "matrix"))






