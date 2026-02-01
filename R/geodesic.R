#' @name geodesic_utils
#' @title Geodesic Distance Utilities
#' @description Core helpers for vertex- and parcel-level geodesic computations
#'   on neurosurf surfaces. Distances are computed on the surface graph using
#'   weighted shortest paths (Dijkstra). Results can be chunked to control
#'   memory and optionally cached for repeated queries.
#' @keywords internal
#' @importFrom fastmap fastmap
NULL

.ns_geodesic_cache <- fastmap::fastmap()

`%||%` <- function(a, b) if (!is.null(a)) a else b

# internal: normalize any supported surface object to an igraph
.as_graph <- function(x) {
  if (inherits(x, "igraph")) {
    return(x)
  }
  graph(x)
}

# internal: validate and return edge weights
.edge_weights <- function(g, weights) {
  if (is.null(weights)) {
    weights <- igraph::E(g)$dist
  }
  if (length(weights) != igraph::ecount(g)) {
    stop("weights length must equal number of edges in graph.")
  }
  if (!is.numeric(weights) || any(is.na(weights))) {
    stop("weights must be numeric and non-missing.")
  }
  as.numeric(weights)
}

# internal: build a lightweight hash for caching
.weight_hash <- function(weights) {
  digest::digest(c(
    length(weights),
    round(range(weights), 6),
    round(mean(weights), 6),
    round(stats::sd(weights), 6)
  ))
}

#' Clear geodesic cache
#'
#' @return TRUE (invisibly)
#'
#' @examples
#' clear_geodesic_cache()
#'
#' @export
clear_geodesic_cache <- function() {
  .ns_geodesic_cache$reset()
  invisible(TRUE)
}

#' All-pairs geodesic distance matrix (chunked)
#'
#' @param geometry \code{SurfaceGeometry}, \code{LabeledNeuroSurface},
#'   \code{NeuroSurface}, or an \code{igraph}.
#' @param vertices Optional integer vector of source vertices. Defaults to
#'   all vertices.
#' @param targets Optional integer vector of target vertices. Defaults to
#'   \code{vertices} (square matrix).
#' @param weights Optional numeric edge weights (defaults to \code{E(g)$dist}).
#' @param mode Output mode: \code{"sparse"} (default, \code{dgCMatrix}) or
#'   \code{"dense"}.
#' @param chunk_size Number of source vertices per Dijkstra batch.
#' @param cache Logical; cache symmetric results when \code{targets} is NULL
#'   or identical to \code{vertices}.
#' @param cache_key Optional manual cache key.
#' @param algorithm Only "dijkstra" is currently implemented.
#'
#' @return A distance matrix (sparse or dense) of size
#'   \code{length(vertices) x length(targets)}.
#'
#' @examples
#' geom <- example_surface_geometry()
#' dmat <- geodesic_distance_matrix(geom, vertices = 1:2)
#'
#' @export
geodesic_distance_matrix <- function(geometry,
                                     vertices = NULL,
                                     targets = NULL,
                                     weights = NULL,
                                     mode = c("sparse", "dense"),
                                     chunk_size = 2000,
                                     cache = TRUE,
                                     cache_key = NULL,
                                     algorithm = c("dijkstra")) {
  mode <- match.arg(mode)
  algorithm <- match.arg(algorithm)
  g <- .as_graph(geometry)

  if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size < 1) {
    stop("chunk_size must be a positive number.")
  }
  chunk_size <- as.integer(ceiling(chunk_size))

  vcount <- igraph::vcount(g)
  if (is.null(vertices)) {
    vertices <- seq_len(vcount)
  }
  if (is.null(targets)) {
    targets <- vertices
  }

  if (!is.numeric(vertices) || any(vertices %% 1 != 0)) {
    stop("vertices must be integer indices.")
  }
  if (!is.numeric(targets) || any(targets %% 1 != 0)) {
    stop("targets must be integer indices.")
  }
  if (any(vertices < 1) || any(vertices > vcount)) {
    stop("vertices indices out of range.")
  }
  if (any(targets < 1) || any(targets > vcount)) {
    stop("targets indices out of range.")
  }

  weights <- .edge_weights(g, weights)

  symmetric_query <- length(vertices) == length(targets) &&
    identical(vertices, targets)

  use_cache <- cache && symmetric_query && mode == "sparse"

  key <- NULL
  if (use_cache) {
    key <- cache_key %||% digest::digest(list(
      v = vertices,
      h = .weight_hash(weights),
      c = chunk_size,
      a = algorithm,
      n = vcount
    ))
    cached <- .ns_geodesic_cache$get(key)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  n_src <- length(vertices)
  n_tgt <- length(targets)

  if (mode == "dense") {
    out <- matrix(Inf, n_src, n_tgt)
  } else {
    i_idx <- integer(0)
    j_idx <- integer(0)
    x_val <- numeric(0)
  }

  src_seq <- seq(1, n_src, by = chunk_size)
  for (start in src_seq) {
    end <- min(n_src, start + chunk_size - 1)
    src_idx <- start:end
    src <- vertices[src_idx]

    d_chunk <- igraph::distances(
      g,
      v = src,
      to = targets,
      weights = weights,
      algorithm = algorithm
    )

    if (mode == "dense") {
      out[src_idx, ] <- d_chunk
    } else {
      nz <- which(is.finite(d_chunk), arr.ind = TRUE)
      if (nrow(nz) > 0) {
        i_idx <- c(i_idx, src_idx[nz[, 1]])
        j_idx <- c(j_idx, nz[, 2])
        x_val <- c(x_val, d_chunk[nz])
      }
    }
  }

  if (mode == "dense") {
    res <- out
  } else {
    res <- Matrix::sparseMatrix(
      i = i_idx,
      j = j_idx,
      x = x_val,
      dims = c(n_src, n_tgt)
    )
  }

  if (use_cache) {
    .ns_geodesic_cache$set(key, res)
  }
  res
}

#' Geodesic distances from sources to targets
#'
#' Convenience wrapper returning a numeric vector/matrix.
#'
#' @inheritParams geodesic_distance_matrix
#' @param sources Integer vector of source vertices.
#' @param targets Optional target vertices (defaults to \code{sources}).
#'
#' @return Numeric vector if one source, otherwise a matrix.
#'
#' @examples
#' geom <- example_surface_geometry()
#' d <- geodesic_distances(geom, sources = 1, targets = 2:4)
#'
#' @export
geodesic_distances <- function(geometry,
                               sources,
                               targets = NULL,
                               weights = NULL,
                               algorithm = c("dijkstra"),
                               chunk_size = 2000) {
  algorithm <- match.arg(algorithm)
  dm <- geodesic_distance_matrix(
    geometry = geometry,
    vertices = sources,
    targets = targets,
    weights = weights,
    mode = "dense",
    chunk_size = chunk_size,
    cache = FALSE,
    algorithm = algorithm
  )
  if (length(sources) == 1) {
    return(as.vector(dm))
  }
  dm
}

# internal: parcel components handling
.parcel_components <- function(lsurf,
                               component_policy = c("error", "largest", "each",
                                                    "merge")) {
  component_policy <- match.arg(component_policy)
  g <- .as_graph(lsurf)
  vcount <- igraph::vcount(g)

  labels <- lsurf@data
  idx <- lsurf@indices

  if (length(labels) != length(idx)) {
    stop("length of labels must equal length of indices in LabeledNeuroSurface.")
  }

  vertex_labels <- rep(NA_integer_, vcount)
  vertex_labels[idx] <- as.integer(labels)

  parcels <- sort(unique(vertex_labels[!is.na(vertex_labels)]))
  units <- list()
  vertices_per_unit <- list()
  vtou <- rep(NA_integer_, vcount)

  unit_id <- 1L
  fragmented <- integer(0)

  for (pid in parcels) {
    verts <- which(vertex_labels == pid)
    subg <- igraph::induced_subgraph(g, verts)
    comps <- igraph::components(subg)
    ncomp <- comps$no
    comp_members <- split(verts, comps$membership)

    if (component_policy == "largest" && ncomp > 1) {
      keep <- which.max(comps$csize)
      comp_members <- comp_members[keep]
      ncomp <- 1
    } else if (component_policy == "merge" && ncomp > 1) {
      comp_members <- list(unlist(comp_members, use.names = FALSE))
      ncomp <- 1
    }

    if (component_policy == "error" && ncomp > 1) {
      fragmented <- c(fragmented, pid)
      next
    }

    for (k in seq_along(comp_members)) {
      comp_name <- if (component_policy == "each" && ncomp > 1) {
        paste0(pid, "_c", k)
      } else {
        as.character(pid)
      }
      units[[unit_id]] <- data.frame(
        unit = comp_name,
        parcel_id = pid,
        component = k,
        size = length(comp_members[[k]]),
        stringsAsFactors = FALSE
      )
      vertices_per_unit[[unit_id]] <- comp_members[[k]]
      vtou[comp_members[[k]]] <- unit_id
      unit_id <- unit_id + 1L
    }
  }

  if (length(fragmented) > 0) {
    stop(
      "Fragmented parcels detected: ",
      paste(fragmented, collapse = ", "),
      ". Set component_policy to 'largest', 'each', or 'merge' to proceed."
    )
  }

  units_df <- do.call(rbind, units)
  if (is.null(units_df)) {
    units_df <- data.frame(
      unit = character(),
      parcel_id = integer(),
      component = integer(),
      size = integer(),
      stringsAsFactors = FALSE
    )
  }
  list(
    units = units_df,
    vertices = vertices_per_unit,
    vertex_to_unit = vtou
  )
}

#' Parcel centroids using geodesic medoids
#'
#' @param labeled_surface A \code{LabeledNeuroSurface}.
#' @param method \code{"medoid"} (geodesic mean distance) or \code{"euclidean"}
#'   (closest to Euclidean centroid).
#' @param component_policy How to handle fragmented parcels:
#'   \code{"error"}, \code{"largest"}, \code{"each"}, \code{"merge"}.
#' @inheritParams geodesic_distance_matrix
#'
#' @return Data frame with one row per parcel unit:
#'   \code{unit}, \code{parcel_id}, \code{component}, \code{size},
#'   \code{centroid_vertex}, and Cartesian coordinates.
#'
#' @examples
#' \donttest{
#' # Requires a LabeledNeuroSurface
#' # lsurf <- read_freesurfer_annot("lh.aparc.annot", geom)
#' # centroids <- parcel_geodesic_centroid(lsurf)
#' }
#'
#' @export
parcel_geodesic_centroid <- function(labeled_surface,
                                     method = c("medoid", "euclidean"),
                                     component_policy = c("error", "largest",
                                                          "each", "merge"),
                                     weights = NULL,
                                     chunk_size = 2000,
                                     cache = TRUE) {
  method <- match.arg(method)
  info <- .parcel_components(labeled_surface, component_policy)
  g <- .as_graph(labeled_surface)
  coords_mat <- coords(labeled_surface)

  centroids <- lapply(seq_along(info$vertices), function(i) {
    verts <- info$vertices[[i]]
    if (length(verts) == 1) {
      cv <- verts
    } else if (method == "euclidean") {
      center <- colMeans(coords_mat[verts, , drop = FALSE])
      d <- sqrt(rowSums((coords_mat[verts, , drop = FALSE] - center) ^ 2))
      cv <- verts[which.min(d)]
    } else {
      dm <- geodesic_distance_matrix(
        labeled_surface,
        vertices = verts,
        targets = verts,
        weights = weights,
        mode = "dense",
        chunk_size = chunk_size,
        cache = cache
      )
      mrow <- rowMeans(dm)
      cv <- verts[which.min(mrow)]
    }
    cbind(
      centroid_vertex = cv,
      x = coords_mat[cv, 1],
      y = coords_mat[cv, 2],
      z = coords_mat[cv, 3]
    )
  })

  cent_df <- cbind(
    info$units,
    do.call(rbind, centroids)
  )
  rownames(cent_df) <- NULL
  cent_df
}

#' Parcel-to-parcel geodesic distances
#'
#' @param labeled_surface A \code{LabeledNeuroSurface}.
#' @param metric \code{"centroid"} (distance between parcel medoids) or
#'   \code{"min"} (minimum distance between any vertices of two parcels).
#' @param component_policy Fragment handling policy.
#' @inheritParams geodesic_distance_matrix
#'
#' @return A numeric matrix with parcel-unit names as dimnames.
#'
#' @examples
#' \donttest{
#' # Requires a LabeledNeuroSurface
#' # lsurf <- read_freesurfer_annot("lh.aparc.annot", geom)
#' # dmat <- parcel_geodesic_distance_matrix(lsurf)
#' }
#'
#' @export
parcel_geodesic_distance_matrix <- function(labeled_surface,
                                            metric = c("centroid", "min"),
                                            component_policy = c("error",
                                                                 "largest",
                                                                 "each",
                                                                 "merge"),
                                            weights = NULL,
                                            chunk_size = 2000,
                                            cache = TRUE) {
  metric <- match.arg(metric)
  component_policy <- match.arg(component_policy)
  info <- .parcel_components(labeled_surface, component_policy)

  if (metric == "centroid") {
    cent <- parcel_geodesic_centroid(
      labeled_surface,
      component_policy = component_policy,
      weights = weights,
      chunk_size = chunk_size,
      cache = cache
    )
    centers <- cent$centroid_vertex
    dm <- geodesic_distance_matrix(
      labeled_surface,
      vertices = centers,
      targets = centers,
      weights = weights,
      mode = "dense",
      chunk_size = chunk_size,
      cache = cache
    )
    dimnames(dm) <- list(cent$unit, cent$unit)
    return(dm)
  }

  # metric == "min"
  g <- .as_graph(labeled_surface)
  weights <- .edge_weights(g, weights)

  n_units <- nrow(info$units)
  out <- matrix(Inf, n_units, n_units)
  diag(out) <- 0

  all_targets <- unique(unlist(info$vertices))
  target_lookup <- lapply(info$vertices, function(v) {
    match(v, all_targets)
  })

  for (i in seq_len(n_units)) {
    src <- info$vertices[[i]]
    dmat <- geodesic_distance_matrix(
      labeled_surface,
      vertices = src,
      targets = all_targets,
      weights = weights,
      mode = "dense",
      chunk_size = chunk_size,
      cache = FALSE
    )
    for (j in i:n_units) {
      dmin <- min(dmat[, target_lookup[[j]]], na.rm = TRUE)
      if (!is.finite(dmin)) {
        dmin <- Inf
      }
      out[i, j] <- dmin
      out[j, i] <- dmin
    }
  }
  dimnames(out) <- list(info$units$unit, info$units$unit)
  out
}

#' Parcel boundary contact matrix
#'
#' @param labeled_surface A \code{LabeledNeuroSurface}.
#' @param component_policy Fragment handling policy.
#' @param counts If TRUE, return edge counts; otherwise logical contact matrix.
#'
#' @return Matrix (logical or integer) indicating parcel-unit boundary contacts.
#'
#' @examples
#' \donttest{
#' # Requires a LabeledNeuroSurface
#' # lsurf <- read_freesurfer_annot("lh.aparc.annot", geom)
#' # contact <- parcel_boundary_contact(lsurf)
#' }
#'
#' @export
parcel_boundary_contact <- function(labeled_surface,
                                    component_policy = c("error", "largest",
                                                         "each", "merge"),
                                    counts = FALSE) {
  component_policy <- match.arg(component_policy)
  info <- .parcel_components(labeled_surface, component_policy)
  g <- .as_graph(labeled_surface)

  edges <- igraph::as_edgelist(g, names = FALSE)
  vtou <- info$vertex_to_unit

  u1 <- vtou[edges[, 1]]
  u2 <- vtou[edges[, 2]]
  keep <- !is.na(u1) & !is.na(u2) & u1 != u2
  if (!any(keep)) {
    mat <- matrix(if (counts) 0 else FALSE,
                  nrow = nrow(info$units),
                  ncol = nrow(info$units))
    dimnames(mat) <- list(info$units$unit, info$units$unit)
    return(mat)
  }

  pairs <- cbind(u1[keep], u2[keep])
  pairs <- t(apply(pairs, 1, sort))

  key <- paste(pairs[, 1], pairs[, 2], sep = "-")
  tab <- tapply(rep(1L, length(key)), key, sum)

  n_units <- nrow(info$units)
  mat <- matrix(0, n_units, n_units)
  for (nm in names(tab)) {
    ij <- as.integer(strsplit(nm, "-", fixed = TRUE)[[1]])
    mat[ij[1], ij[2]] <- tab[[nm]]
    mat[ij[2], ij[1]] <- tab[[nm]]
  }
  if (!counts) {
    mat <- mat > 0
  }
  dimnames(mat) <- list(info$units$unit, info$units$unit)
  mat
}
