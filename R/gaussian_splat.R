#' Gaussian splats on surface meshes
#'
#' Create per-vertex Gaussian falloff maps centred on coordinates or
#' vertices. Distances can be measured either in straight-line Euclidean
#' space or along the mesh using geodesic paths (Dijkstra on edge lengths).
#'
#' @param surface A \code{SurfaceGeometry}.
#' @param center Numeric vector of length 3 giving the centre coordinate.
#' @param sigma Positive numeric standard deviation of the Gaussian kernel.
#' @param use_geodesic Logical; when \code{TRUE} use along-surface distances.
#' @param vertex_idx Integer index (1-based) of the centre vertex.
#' @param centers Numeric matrix with three columns; each row is a centre.
#' @param sigmas Numeric vector of length 1 or \code{nrow(centers)}.
#' @param weights Optional numeric vector of length 1 or
#'   \code{nrow(centers)} used to weight each centre (defaults to 1).
#'
#' @return A \code{NeuroSurface} with values
#'   \eqn{\exp(-d^2 / (2 * sigma^2))} at every vertex. For
#'   \code{gaussian_splat_multi} the per-centre maps are summed (after
#'   weighting).
#'
#' @examples
#' geom <- example_surface_geometry()
#' g1 <- gaussian_splat(geom, center = c(0, 0, 0), sigma = 1)
#' v_idx <- find_nearest_vertex(geom, c(0.1, 0, 0))
#' g2 <- gaussian_splat_vertex(geom, vertex_idx = v_idx, sigma = 1)
#' g_multi <- gaussian_splat_multi(
#'   geom,
#'   centers = rbind(c(0, 0, 0), c(1, 0, 0)),
#'   sigmas = c(0.5, 1)
#' )
#' @export
gaussian_splat <- function(surface, center, sigma, use_geodesic = FALSE) {
  geom <- .validate_surface(surface)
  center <- .validate_center(center)
  sigma <- .validate_sigma_scalar(sigma)
  use_geodesic <- .validate_logical_flag(use_geodesic, "use_geodesic")

  d <- if (use_geodesic) {
    v_idx <- find_nearest_vertex(geom, center)
    .geodesic_from_vertex(geom, v_idx)
  } else {
    .euclidean_distance_to_vertices(geom, center)
  }

  NeuroSurface(
    geom,
    indices = nodes(geom),
    data = .gaussian_kernel(d, sigma)
  )
}

#' @rdname gaussian_splat
#' @export
gaussian_splat_vertex <- function(surface,
                                  vertex_idx,
                                  sigma,
                                  use_geodesic = FALSE) {
  geom <- .validate_surface(surface)
  vcount <- igraph::vcount(graph(geom))
  vertex_idx <- .validate_vertex_index(vertex_idx, vcount)
  sigma <- .validate_sigma_scalar(sigma)
  use_geodesic <- .validate_logical_flag(use_geodesic, "use_geodesic")

  d <- if (use_geodesic) {
    .geodesic_from_vertex(geom, vertex_idx)
  } else {
    center <- coords(geom)[vertex_idx, ]
    .euclidean_distance_to_vertices(geom, center)
  }

  NeuroSurface(
    geom,
    indices = nodes(geom),
    data = .gaussian_kernel(d, sigma)
  )
}

#' @rdname gaussian_splat
#' @export
gaussian_splat_multi <- function(surface,
                                 centers,
                                 sigmas,
                                 weights = NULL,
                                 use_geodesic = FALSE) {
  geom <- .validate_surface(surface)
  centers <- .validate_centers_matrix(centers)
  n_centers <- nrow(centers)
  if (n_centers == 0) {
    stop("centers must contain at least one row.")
  }

  sigmas <- .normalize_sigmas(sigmas, n_centers)
  weights <- .normalize_weights(weights, n_centers)
  use_geodesic <- .validate_logical_flag(use_geodesic, "use_geodesic")

  dist_mat <- if (use_geodesic) {
    center_vertices <- apply(
      centers,
      1,
      function(pt) find_nearest_vertex(geom, pt)
    )
    g <- graph(geom)
    igraph::distances(
      g,
      v = center_vertices,
      to = igraph::V(g),
      weights = igraph::E(g)$dist,
      algorithm = "dijkstra"
    )
  } else {
    .euclidean_distance_matrix(geom, centers)
  }

  sigma_mat <- matrix(2 * sigmas ^ 2, nrow = n_centers,
                      ncol = ncol(dist_mat))
  gauss_mat <- exp(-(dist_mat ^ 2) / sigma_mat)
  weight_mat <- matrix(weights, nrow = n_centers, ncol = ncol(gauss_mat))
  values <- colSums(gauss_mat * weight_mat)

  NeuroSurface(geom, indices = nodes(geom), data = values)
}

#' Find the nearest surface vertex to a 3D point
#'
#' @param surface A \code{SurfaceGeometry}.
#' @param point Numeric vector of length 3.
#'
#' @return Integer vertex index (1-based) of the closest vertex.
#' @examples
#' geom <- example_surface_geometry()
#' find_nearest_vertex(geom, c(0.9, 0, 0))
#' @export
find_nearest_vertex <- function(surface, point) {
  geom <- .validate_surface(surface)
  point <- .validate_center(point)
  cds <- coords(geom)
  if (nrow(cds) == 0) {
    stop("surface has no vertices.")
  }
  center_mat <- matrix(point, nrow = nrow(cds), ncol = 3, byrow = TRUE)
  d2 <- rowSums((cds - center_mat) ^ 2)
  as.integer(which.min(d2))
}

.gaussian_kernel <- function(distance, sigma) {
  exp(-(distance ^ 2) / (2 * sigma ^ 2))
}

.euclidean_distance_to_vertices <- function(surface, center) {
  cds <- coords(surface)
  center_mat <- matrix(center, nrow = nrow(cds), ncol = 3, byrow = TRUE)
  sqrt(rowSums((cds - center_mat) ^ 2))
}

.euclidean_distance_matrix <- function(surface, centers) {
  cds <- coords(surface)
  center_norm <- rowSums(centers ^ 2)
  vertex_norm <- rowSums(cds ^ 2)
  dist_sq <- outer(center_norm, vertex_norm, "+") -
    2 * centers %*% t(cds)
  sqrt(pmax(dist_sq, 0))
}

.geodesic_from_vertex <- function(surface, vertex_idx) {
  g <- graph(surface)
  d <- igraph::distances(
    g,
    v = vertex_idx,
    to = igraph::V(g),
    weights = igraph::E(g)$dist,
    algorithm = "dijkstra"
  )
  as.numeric(d[1, ])
}

.validate_surface <- function(surface) {
  if (!inherits(surface, "SurfaceGeometry")) {
    stop("surface must be a SurfaceGeometry.")
  }
  surface
}

.validate_center <- function(center) {
  center <- as.numeric(center)
  if (length(center) != 3 || any(is.na(center))) {
    stop("center must be a numeric vector of length 3.")
  }
  center
}

.validate_centers_matrix <- function(centers) {
  if (is.null(dim(centers))) {
    centers <- matrix(centers, ncol = 3, byrow = TRUE)
  } else {
    centers <- as.matrix(centers)
  }
  if (!is.numeric(centers)) {
    stop("centers must be numeric.")
  }
  if (ncol(centers) != 3) {
    stop("centers must have three columns.")
  }
  if (any(is.na(centers))) {
    stop("centers cannot contain missing values.")
  }
  centers
}

.validate_vertex_index <- function(vertex_idx, n_vertices) {
  if (!is.numeric(vertex_idx) || length(vertex_idx) != 1 ||
      is.na(vertex_idx) || vertex_idx %% 1 != 0) {
    stop("vertex_idx must be a single integer index.")
  }
  vertex_idx <- as.integer(vertex_idx)
  if (vertex_idx < 1 || vertex_idx > n_vertices) {
    stop("vertex_idx is out of range for this surface.")
  }
  vertex_idx
}

.validate_sigma_scalar <- function(sigma) {
  if (!is.numeric(sigma) || length(sigma) != 1 || is.na(sigma) ||
      sigma <= 0) {
    stop("sigma must be a single positive number.")
  }
  as.numeric(sigma)
}

.normalize_sigmas <- function(sigmas, n_centers) {
  if (!is.numeric(sigmas) || any(is.na(sigmas))) {
    stop("sigmas must be numeric and non-missing.")
  }
  if (length(sigmas) == 1) {
    sigmas <- rep(sigmas, n_centers)
  } else if (length(sigmas) != n_centers) {
    stop("sigmas must be length 1 or match the number of centers.")
  }
  if (any(sigmas <= 0)) {
    stop("sigmas must be positive.")
  }
  as.numeric(sigmas)
}

.normalize_weights <- function(weights, n_centers) {
  if (is.null(weights)) {
    return(rep(1, n_centers))
  }
  if (!is.numeric(weights) || any(is.na(weights))) {
    stop("weights must be numeric and non-missing.")
  }
  if (length(weights) == 1) {
    weights <- rep(weights, n_centers)
  } else if (length(weights) != n_centers) {
    stop("weights must be length 1 or match the number of centers.")
  }
  as.numeric(weights)
}

.validate_logical_flag <- function(flag, name) {
  if (!is.logical(flag) || length(flag) != 1 || is.na(flag)) {
    stop(name, " must be a single logical value.")
  }
  flag
}
