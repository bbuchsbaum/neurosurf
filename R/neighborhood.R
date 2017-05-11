findNeighbors <- function(graph, node, radius, edgeWeights, max_order=NULL) {
  if (is.null(max_order)) {
    avg_weight <- mean(edgeWeights)
    max_order <- radius/avg_weight + (2*avg_weight)
  }

  cand <- igraph::ego(graph, order= max_order, nodes=node)[[1]]
  D <- igraph::distances(graph, node, cand, weights=edgeWeights, algorithm="dijkstra")
  keep <- which(D < radius)[-1]
  cand[keep]
}

#' @export
#' @importFrom FNN get.knn
findAllNeighbors <- function(surf, radius, edgeWeights, nodes=NULL, distance_type=c("euclidean", "geodesic", "spherical")) {

  if (inherits(surf, "igraph")) {
    g <- surf
  } else {
    g <- graph(surf)
  }


  distance_type <- match.arg(distance_type)

  avg_weight <- quantile(edgeWeights, .25)

  if (is.null(nodes)) {
    nodes <- igraph::V(g)
  }

  all_can <- FNN::get.knn(coords(surf), k=ceiling((radius+2)/avg_weight)^3)

  cds <- coords(g)

  if (distance_type == "spherical") {
    print("spherical")
    R <- diff(range(cds[,1]))/2
    lat <- asin(cds[,3]/R)
    lon <- atan2(cds[,2], cds[,1])
  }

  nabeinfo <- lapply(nodes, function(v) {
    cand <- c(v, all_can$nn.index[v,])
    if (distance_type == "geodesic") {
      D <- igraph::distances(g, v, cand, weights=edgeWeights, algorithm="dijkstra")
    } else if (distance_type == "euclidean") {
      D <- c(0, all_can$nn.dist[v,])
    } else if (distance_type == "spherical") {
      ind <- all_can$nn.index[v,]
      ang <- acos(sin(lat[v]) * sin(lat[ind]) + cos(lat[v]) * cos(lat[ind]) * cos(abs(lon[v] - lon[ind])))
      D <- c(0, R * ang)
    }

    keep <- which(D < radius)
    if (length(keep) > 0) {
      knabes <- cand[keep]
      cbind(i=rep(v, length(knabes)), j=knabes, d=D[keep])
    } else {
      matrix(0,0,0)
    }
  })

  nabeinfo


}

#' convert an edge list to an 'igraph' instance
.neighbors_to_graph <- function(nabelist) {
  mat <- plyr::rbind.fill.matrix(nabelist)
  g <- igraph::graph.edgelist(mat[,1:2])
  igraph::E(g)$dist <- as.numeric(mat[,3])
  g
}

#' @rdname neighborGraph-methods
#' @importFrom grDevices rainbow
#' @export
#' @aliases neighborGraph,igraph,numeric,missing,missing
setMethod(f="neighborGraph", signature=c(x="igraph", radius="numeric", edgeWeights="missing", nodes="missing"),
          def=function(x, radius, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            edgeWeights=igraph::E(x)$dist
            nabeinfo <- findAllNeighbors(x, radius, as.vector(edgeWeights), distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })




#' @rdname neighborGraph-methods
#' @importFrom grDevices rainbow
#' @export
#' @aliases neighborGraph,igraph,numeric,missing,missing
setMethod(f="neighborGraph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="missing", nodes="missing"),
          def=function(x, radius, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            edgeWeights=igraph::E(graph(x))$dist
            nabeinfo <- findAllNeighbors(x, radius, as.vector(edgeWeights), distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })



#' @rdname neighborGraph-methods
#' @export
#' @aliases neighborGraph,igraph,numeric,numeric,missing
setMethod(f="neighborGraph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="numeric", nodes="missing"),
          def=function(x, radius, edgeWeights, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            stopifnot(length(edgeWeights) == length(igraph::E(graph(x))))
            nabeInfo <- findAllNeighbors(x, radius, edgeWeights, distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })



#' @rdname neighborGraph-methods
#' @export
#' @aliases neighborGraph,igraph,numeric,numeric,integer
setMethod(f="neighborGraph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="numeric", nodes="integer"),

          def=function(x,radius, edgeWeights, nodes, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            assert_that(length(edgeWeights) == length(igraph::E(graph(x))))
            nabeInfo <- findAllNeighbors(x,radius, edgeWeights, nodes, distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })

#' @rdname neighborGraph-methods
#' @export
#' @aliases neighborGraph,igraph,numeric,missing,integer
setMethod(f="neighborGraph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="missing", nodes="integer"),
          def=function(x,radius, nodes, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            nabeinfo <- findAllNeighbors(x, radius, igraph::E(graph(x))$dist, nodes,distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })


#' laplacian
#'
#' @rdname laplacian-methods
#' @export
setMethod(f="laplacian", signature=c(x="SurfaceGeometry", normalized="missing", weights="missing"),
          def=function(x) {
            igraph::laplacian_matrix(graph(x))
          })

#' laplacian
#'
#' @rdname laplacian-methods
#' @export
setMethod(f="laplacian", signature=c(x="SurfaceGeometry", normalized="missing", weights="numeric"),
          def=function(x, weights) {
            igraph::laplacian_matrix(neurosurf::graph(x), weights=weights)
          })

#' adjacency
#'
#' @rdname adjacency-methods
#' @export
setMethod(f="adjacency", signature=c(x="SurfaceGeometry", attr="numeric"),
          def=function(x, attr) {
            g <- graph(x)
            igraph::E(g)$awt <- attr
            igraph::as_adjacency_matrix(g, attr="awt")
          })

#' adjacency
#'
#' @rdname adjacency-methods
#' @export
setMethod(f="adjacency", signature=c(x="SurfaceGeometry", attr="character"),
          def=function(x, attr) {
            igraph::as_adjacency_matrix(graph(x), attr=attr)
          })

#' adjacency
#'
#' @rdname adjacency-methods
#' @export
setMethod(f="adjacency", signature=c(x="SurfaceGeometry", attr="missing"),
          def=function(x) {
            igraph::as_adjacency_matrix(graph(x))
          })

setMethod(f="smooth", signature=c(x="BrainSurface", sigma="missing"),
           def=function(x, sigma, ...) {
             g <- graph(geometry(bsurf))
             ind <- indices(x)
             vlist <- igraph::adjacent_vertices(g, indices(x))
             cds <- coords(x)

             smat <- parallel::mclapply(1:length(vlist), function(i) {
               print(i)
               m <- series(x, c(ind[i], vlist[[i]]))
               Matrix::rowMeans(m)
             }, mc.cores=parallel::detectCores())
           })