#' @include all_class.R
#' @include all_generic.R
NULL

#' Find Neighboring Nodes Within a Specified Radius
#'
#' @description
#' Internal function that identifies all nodes within a specified radius from a given node in a graph.
#'
#' @param graph An igraph object representing the mesh connectivity
#' @param node Integer; the index of the node for which to find neighbors
#' @param radius Numeric; the maximum distance within which to consider nodes as neighbors
#' @param edgeWeights Numeric vector; weights for edges used in distance computation
#' @param max_order Integer; maximum order of neighborhood to consider. If NULL, it's calculated
#'                 based on radius and average edge weight
#'
#' @details
#' The function first identifies candidate neighbors using igraph's ego function up to max_order,
#' then filters these candidates based on the actual distance computed using Dijkstra's algorithm.
#' The max_order parameter is automatically calculated if not provided, using the formula:
#' radius/avg_weight + (2*avg_weight), which provides a reasonable upper bound.
#'
#' @return Integer vector of node indices that are within the specified radius of the input node
#'
#' @noRd
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

#' Find Node Neighbors in a Surface Mesh
#'
#' @description
#' Identifies all neighboring nodes within a specified radius for a given surface mesh.
#'
#' @param surf A SurfaceGeometry object or igraph object representing the mesh
#' @param radius Numeric; the spatial radius within which to search for neighbors
#' @param edgeWeights Numeric vector; weights for edges used in distance computation
#' @param nodes Integer vector; subset of nodes to find neighbors for. If NULL, uses all nodes
#' @param distance_type Character; type of distance metric to use: "euclidean", "geodesic", or "spherical"
#'
#' @return A list of matrices, each containing neighbor information:
#'   \item{i}{Source node index}
#'   \item{j}{Neighbor node index}
#'   \item{d}{Distance between nodes}
#'
#' @details
#' The function supports three distance metrics: Euclidean, geodesic, and spherical.
#' For spherical distances, the surface is assumed to be a sphere.
#'
#' @importFrom FNN get.knn
#' @importFrom stats quantile
#' @importFrom igraph V distances
#'
#' @examples
#' # Load a sample inflated surface from the package
#' surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
#' surf <- readAsc(surf_file)
#' 
#' # Create edge weights (using uniform weights for simplicity)
#' g <- graph(surf)
#' edge_weights <- rep(1, length(E(g)))
#' 
#' # Find neighbors within a 10mm radius for the first 5 vertices
#' neighbors <- find_all_neighbors(surf, radius = 10, 
#'                                edgeWeights = edge_weights,
#'                                nodes = 1:5,
#'                                distance_type = "geodesic")
#' 
#' # Check the number of neighbors found for the first vertex
#' nrow(neighbors[[1]])
#' 
#' # Look at the first few neighbors of the first vertex
#' head(neighbors[[1]])
#'
#' @export
find_all_neighbors <- function(surf, radius, edgeWeights, nodes=NULL,
                               distance_type=c("euclidean", "geodesic", "spherical")) {
  ## check out geosphere package
  if (inherits(surf, "igraph")) {
    g <- surf
  } else {
    g <- graph(surf)
  }

  distance_type <- match.arg(distance_type)

  avg_weight <- stats::quantile(edgeWeights, .25)

  if (is.null(nodes)) {
    nodes <- igraph::V(g)
  }

  all_can <- FNN::get.knn(coords(surf), k=ceiling((radius+2)/avg_weight)^3)

  cds <- coords(g)

  if (distance_type == "spherical") {
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

# convert an edge list to an 'igraph' instance
#' @importFrom plyr rbind.fill.matrix
#' @noRd
.neighbors_to_graph <- function(nabelist) {
  mat <- plyr::rbind.fill.matrix(nabelist)
  g <- igraph::graph.edgelist(mat[,1:2])
  igraph::E(g)$dist <- as.numeric(mat[,3])
  g
}

#' Create Neighbor Graph from igraph Object
#'
#' @description
#' Constructs a neighbor graph from an igraph object based on a specified radius.
#'
#' @param x An igraph object representing the original graph
#' @param radius Numeric; the spatial radius within which to consider nodes as neighbors
#' @param distance_type Character; type of distance metric to use: "geodesic", "euclidean", or "spherical"
#'
#' @return An igraph object representing the neighbor graph
#'
#' @details
#' This method creates a neighbor graph by finding all nodes within the specified radius
#' for each node in the input graph. Edge weights are taken from the 'dist' attribute
#' of the input graph's edges.
#'
#' @seealso \code{\link{find_all_neighbors}}, \code{\link{.neighbors_to_graph}}
#'
#' @examples
#' \donttest{
#' g <- make_graph("Zachary")
#' E(g)$dist <- runif(ecount(g))
#' neighbor_g <- neighbor_graph(g, radius = 0.5, distance_type = "geodesic")
#' }
#'
#' @importFrom igraph E
#' @exportMethod neighbor_graph
#' @aliases neighbor_graph,igraph,numeric,missing,missing
setMethod(f="neighbor_graph", signature=c(x="igraph", radius="numeric", edgeWeights="missing", nodes="missing"),
          def=function(x, radius, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            edgeWeights=igraph::E(x)$dist
            nabeinfo <- find_all_neighbors(x, radius, as.vector(edgeWeights),
                                           distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })




#' @rdname neighbor_graph
#' @export
#' @aliases neighbor_graph,igraph,numeric,missing,missing
setMethod(f="neighbor_graph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="missing", nodes="missing"),
          def=function(x, radius, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            edgeWeights=igraph::E(graph(x))$dist
            nabeinfo <- find_all_neighbors(x, radius, as.vector(edgeWeights),
                                           distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })



#' @rdname neighbor_graph
#' @export
#' @aliases neighbor_graph,igraph,numeric,numeric,missing
setMethod(f="neighbor_graph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="numeric", nodes="missing"),
          def=function(x, radius, edgeWeights, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            stopifnot(length(edgeWeights) == length(igraph::E(graph(x))))
            nabeinfo <- find_all_neighbors(x, radius, edgeWeights, distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })



#' @rdname neighbor_graph
#' @export
#' @aliases neighbor_graph,igraph,numeric,numeric,integer
setMethod(f="neighbor_graph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="numeric", nodes="integer"),

          def=function(x,radius, edgeWeights, nodes, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            assert_that(length(edgeWeights) == length(igraph::E(graph(x))))
            nabeinfo <- find_all_neighbors(x,radius, edgeWeights, nodes, distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })

#' @rdname neighbor_graph
#' @export
#' @aliases neighbor_graph,igraph,numeric,missing,integer
setMethod(f="neighbor_graph", signature=c(x="SurfaceGeometry", radius="numeric", edgeWeights="missing", nodes="integer"),
          def=function(x,radius, nodes, distance_type=c("geodesic", "euclidean", "spherical")) {
            distance_type <- match.arg(distance_type)
            nabeinfo <- find_all_neighbors(x, radius, igraph::E(graph(x))$dist, nodes,distance_type=distance_type)
            .neighbors_to_graph(nabeinfo)
          })



#' @export
#' @rdname laplacian
setMethod(f="laplacian", signature=c(x="SurfaceGeometry", normalized="missing", weights="missing"),
          def=function(x) {
            igraph::laplacian_matrix(graph(x))
          })


#' @export
#' @rdname laplacian
setMethod(f="laplacian", signature=c(x="SurfaceGeometry", normalized="missing", weights="numeric"),
          def=function(x, weights) {
            igraph::laplacian_matrix(neurosurf::graph(x), weights=weights)
          })


#' @export
#' @rdname adjacency
setMethod(f="adjacency", signature=c(x="SurfaceGeometry", attr="numeric"),
          def=function(x, attr) {
            g <- graph(x)
            igraph::E(g)$awt <- attr
            igraph::as_adjacency_matrix(g, attr="awt")
          })


#' @export
#' @rdname adjacency
setMethod(f="adjacency", signature=c(x="SurfaceGeometry", attr="character"),
          def=function(x, attr) {
            igraph::as_adjacency_matrix(graph(x), attr=attr)
          })


#' @export
#' @rdname adjacency
setMethod(f="adjacency", signature=c(x="SurfaceGeometry", attr="missing"),
          def=function(x) {
            igraph::as_adjacency_matrix(graph(x))
          })


#' Smooth a Brain Surface Geometry
#'
#' This method applies smoothing to a brain surface geometry object of class \code{SurfaceGeometry} using various algorithms. Smoothing is useful for removing noise and creating a more continuous surface.
#'
#' @param x A \code{\linkS4class{SurfaceGeometry}} object representing the brain surface to be smoothed.
#' @param type A character string specifying the smoothing algorithm to use. Available options are:
#' \describe{
#'   \item{"taubin"}{Applies Taubin smoothing, which preserves the overall shape of the surface while reducing noise.}
#'   \item{"laplace"}{Performs Laplacian smoothing, which is a basic smoothing method that averages the position of each vertex with its neighbors.}
#'   \item{"HClaplace"}{Applies a Laplacian smoothing with hard constraints. It preserves the boundary vertices and is useful for surfaces with important edge features.}
#'   \item{"fujiLaplace"}{Uses a Laplacian smoothing method that preserves features more aggressively than the basic Laplacian method.}
#'   \item{"angWeight"}{Performs angle-weighted smoothing, which considers the angles between faces to preserve sharp features.}
#'   \item{"surfPreserveLaplace"}{Applies surface-preserving Laplacian smoothing, aiming to maintain the original surface's key characteristics while smoothing.}
#' }
#' @param lambda A numeric value that controls the amount of smoothing. Higher values lead to more aggressive smoothing. This parameter is particularly relevant for Taubin and Laplacian smoothing methods.
#' @param mu A numeric value used in Taubin smoothing to control shrinkage. A value close to zero reduces shrinkage, while a negative value can help in shape preservation.
#' @param delta A numeric value used in certain smoothing algorithms to adjust the influence of smoothing (e.g., in surface-preserving methods).
#' @param iteration An integer specifying the number of smoothing iterations to apply. More iterations result in a smoother surface but can also lead to excessive flattening.
#'
#' @return The function returns the smoothed \code{SurfaceGeometry} object with the updated mesh.
#'
#' @examples
#' # Load a surface file from the extdata directory
#' surf_file <- system.file("extdata", "sample_surface.asc", package = "neurosurf")
#' surface <- readAsc(surf_file)
#' 
#' # Apply Taubin smoothing to the brain surface
#' smoothed_surface1 <- smooth(surface, type = "taubin", lambda = 0.5, mu = -0.5, iteration = 10)
#' 
#' # Apply surface-preserving Laplacian smoothing
#' smoothed_surface2 <- smooth(surface, type = "surfPreserveLaplace", iteration = 5)
#'
#' @importFrom Rvcg vcgSmooth
#' @seealso \code{\link[Rvcg]{vcgSmooth}} for more details on the underlying smoothing algorithms.
#' @export
setMethod(f="smooth", signature=c(x="SurfaceGeometry"),
          def=function(x, type=c("taubin","laplace","HClaplace","fujiLaplace","angWeight","surfPreserveLaplace"),
                       lambda=.7, mu=-.53, delta=.1, iteration=5) {
            smesh <- Rvcg::vcgSmooth(x@mesh, type=type, lambda=lambda, mu=mu, delta=delta, iteration=iteration)
            x@mesh <- smesh
            x
          })

#' Smooth Data on a NeuroSurface Object
#'
#' This method applies smoothing to the data values associated with a \code{\linkS4class{NeuroSurface}} object. Unlike the geometric smoothing applied to \code{\linkS4class{SurfaceGeometry}}, this function smooths the scalar values (e.g., intensity or activation) associated with each vertex on the surface.
#'
#' @param x A \code{\linkS4class{NeuroSurface}} object containing the brain surface and associated data to be smoothed.
#' @param sigma A numeric value specifying the smoothing radius. This defines the neighborhood around each vertex used to compute the smoothed value. Default is 5.
#' @param ... Additional arguments passed to the smoothing function.
#'
#' @return A new \code{NeuroSurface} object with the smoothed data values. The geometry remains unchanged.
#'
#' @details
#' The smoothing process involves averaging the data values within the neighborhood of each vertex. For each vertex on the surface, the function calculates the mean of its own value and the values of its adjacent vertices within the graph structure of the surface. The result is a smoother representation of the data, which can be useful for reducing noise or visualizing broader trends on the surface.
#'
#' The smoothing is particularly useful when working with noisy data or when a smoother representation of the underlying signal is desired. It is commonly applied in neuroimaging to enhance visualization or prepare data for further analysis.
#'
#' @examples
#' # Load a surface file from the extdata directory
#' surf_file <- system.file("extdata", "sample_surface.asc", package = "neurosurf")
#' surface <- readAsc(surf_file)
#' 
#' # Create some random data for the surface vertices
#' n_vertices <- nrow(coords(surface))
#' random_data <- rnorm(n_vertices)
#' 
#' # Create a NeuroSurface object with the surface and data
#' neuro_surf <- NeuroSurface(geometry = surface, 
#'                           indices = 1:n_vertices,
#'                           data = random_data)
#' 
#' # Apply smoothing to the data
#' smoothed_data_surface <- smooth(neuro_surf, sigma = 3)
#' 
#' # The original geometry is preserved, but the data is smoothed
#' # Compare a small section of data before and after smoothing
#' head(random_data)
#' head(series(smoothed_data_surface))
#'
#' @seealso \code{\link{smooth,SurfaceGeometry-method}} for smoothing the geometry of a surface.
#' @export
setMethod(f="smooth", signature=c(x="NeuroSurface"),
           def=function(x, sigma=5, ...) {
             g <- graph(geometry(x))

             ind <- x@indices
             vlist <- igraph::adjacent_vertices(g, ind)
             cds <- coords(x)

             svals <- purrr::map_dbl(1:length(vlist), function(i) {
               m <- series(x, c(ind[i], vlist[[i]]))
               mean(m)
             })

             NeuroSurface(x@geometry, indices=ind, data=unlist(svals))
           })



#' Project 3D Coordinates onto a Surface and Smooth the Values
#'
#' This function projects a set of 3D coordinates onto a given surface and creates a \code{\linkS4class{NeuroSurface}} object with the smoothed values.
#' The projection is performed by finding the closest points on the surface, and then a kernel density smoother is applied locally to produce the final values.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object representing the surface onto which the coordinates will be projected.
#' @param coords A numeric matrix with three columns (x, y, z) representing the 3D coordinates to be projected onto the surface.
#' @param sigma A numeric value specifying the smoothing radius for the kernel density smoother. Default is 5.
#' @param ... Additional arguments passed to the smoothing function.
#'
#' @return A \code{\linkS4class{NeuroSurface}} object with the smoothed values mapped onto the surface.
#'
#' @details
#' The function first projects each 3D coordinate onto the closest point on the surface defined by \code{surfgeom}.
#' The values at these projected points are then smoothed using a kernel density smoother, where the \code{sigma} parameter controls the extent of the smoothing.
#' The result is a \code{NeuroSurface} object containing the smoothed values, suitable for further analysis or visualization.
#'
#' @examples
#' # Load a sample surface from the package
#' surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
#' surfgeom <- readAsc(surf_file)
#' 
#' # Get the surface coordinates
#' surf_coords <- coords(surfgeom)
#' 
#' # Create some sample 3D coordinates to project
#' # We'll use a subset of the surface vertices with small random offsets
#' set.seed(123)
#' sample_indices <- sample(1:nrow(surf_coords), 50)
#' sample_coords <- surf_coords[sample_indices, ] + matrix(rnorm(150, 0, 0.5), ncol = 3)
#' 
#' # Project these coordinates onto the surface
#' projected_surface <- projectCoordinates(surfgeom, sample_coords, sigma = 3)
#' 
#' # Check the result
#' max(series(projected_surface))  # Maximum density value
#' sum(series(projected_surface) > 0)  # Number of vertices with non-zero values
#'
#' @export
projectCoordinates <- function(surfgeom, coords, sigma=5, ...) {
  # Input validation
  if (!inherits(surfgeom, "SurfaceGeometry")) {
    stop("surfgeom must be a SurfaceGeometry object")
  }
  
  if (!is.matrix(coords)) {
    stop("coords must be a matrix")
  }
  
  if (ncol(coords) != 3) {
    stop("coords must have exactly 3 columns (x, y, z)")
  }
  
  if (!is.numeric(sigma) || sigma <= 0) {
    stop("sigma must be a positive number")
  }
  
  # Get the surface vertices
  surf_coords <- coords(surfgeom)
  
  # Check if we have any vertices
  if (nrow(surf_coords) == 0) {
    stop("Surface has no vertices")
  }
  
  # Find the closest vertex on the surface for each coordinate using FNN
  tryCatch({
    nearest_vertices <- FNN::get.knnx(surf_coords, coords, k=1)$nn.index[,1]
  }, error = function(e) {
    stop("Error finding nearest vertices: ", e$message)
  })
  
  # Create initial data vector for the NeuroSurface with counts of nearest points
  n_vertices <- nrow(surf_coords)
  data_vec <- numeric(n_vertices)
  
  # Count occurrences of each vertex
  tabulated_vertices <- table(nearest_vertices)
  vertex_indices <- as.numeric(names(tabulated_vertices))
  
  # Validate indices before assignment
  if (any(vertex_indices > n_vertices) || any(vertex_indices < 1)) {
    stop("Invalid vertex indices found")
  }
  
  # Assign counts to data vector
  data_vec[vertex_indices] <- as.numeric(tabulated_vertices)
  
  # Define valid surface node indices (all vertices)
  valid_indices <- 1:n_vertices
  
  # Create a NeuroSurface object with these values
  tryCatch({
    neuro_surface <- NeuroSurface(geometry = surfgeom, 
                                 indices = valid_indices, 
                                 data = data_vec)
  }, error = function(e) {
    stop("Error creating NeuroSurface object: ", e$message)
  })
  
  # Smooth the values on the NeuroSurface
  tryCatch({
    smooth_surface <- smooth(neuro_surface, sigma = sigma, ...)
  }, error = function(e) {
    stop("Error smoothing surface: ", e$message)
  })
  
  return(smooth_surface)
}
