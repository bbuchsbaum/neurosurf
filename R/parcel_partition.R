# Shared constrained parcel partition. Coordinates used for classification are
# barycentric in the ORIGINAL face; display geometry and cameras do not affect
# labels. See GH #88 for the versioned numerical contract.

.ns_partition_iterations <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 0 || x != floor(x) || x > .Machine$integer.max) {
    stop("'boundary_smooth' must be a non-negative integer.", call. = FALSE)
  }
  as.integer(x)
}

.ns_partition_labels <- function(labels, n) {
  if (!is.numeric(labels) || length(labels) != n || anyNA(labels) ||
      any(!is.finite(labels)) || any(labels < 0) ||
      any(labels != floor(labels)) || any(labels > .Machine$integer.max)) {
    stop("Smooth labels must hold one nonnegative integer per vertex.",
         call. = FALSE)
  }
  as.integer(labels)
}

.ns_partition_clamp <- function(x, tau) {
  if (!is.numeric(x) || length(x) != 2L || any(!is.finite(x)) ||
      x[1] <= 0 || x[1] > 1/3 || x[2] >= 1 ||
      abs(x[2] - (1 - x[1])) > tau || 1 - x[1] >= 1) {
    stop("'t_clamp' must be c(lo, 1-lo), with 0 < lo <= 1/3 and hi < 1.",
         call. = FALSE)
  }
  c(x[1], 1 - x[1])
}

.ns_partition_mesh <- function(vertices, faces) {
  if (!is.numeric(vertices) || ncol(vertices) != 3L ||
      nrow(vertices) < 1L || any(!is.finite(vertices))) {
    stop("vertices must be a finite n x 3 numeric matrix.", call. = FALSE)
  }
  if (!is.numeric(faces) || ncol(faces) != 3L ||
      any(!is.finite(faces)) || any(faces != floor(faces)) ||
      any(faces < 1) || any(faces > nrow(vertices))) {
    stop("faces must contain valid 1-based integer vertex indices.",
         call. = FALSE)
  }
  storage.mode(faces) <- "integer"
  if (any(faces[, 1] == faces[, 2] | faces[, 2] == faces[, 3] |
          faces[, 3] == faces[, 1])) {
    stop("Repeated vertex indices within a face are not supported.",
         call. = FALSE)
  }
  low <- pmin(faces[, 1], faces[, 2], faces[, 3])
  high <- pmax(faces[, 1], faces[, 2], faces[, 3])
  middle <- rowSums(faces) - low - high
  if (anyDuplicated(paste(low, middle, high, sep = "/"))) {
    stop("Duplicate faces are not supported.", call. = FALSE)
  }
  u <- vertices[faces[, 2], , drop = FALSE] -
    vertices[faces[, 1], , drop = FALSE]
  v <- vertices[faces[, 3], , drop = FALSE] -
    vertices[faces[, 1], , drop = FALSE]
  scale <- pmax(abs(u[, 1]), abs(u[, 2]), abs(u[, 3]),
                abs(v[, 1]), abs(v[, 2]), abs(v[, 3]))
  if (any(!is.finite(scale)) || any(scale == 0)) {
    stop("Zero-area faces or unrepresentable edge spans are not supported.",
         call. = FALSE)
  }
  u <- u / scale
  v <- v / scale
  cross <- cbind(u[, 2] * v[, 3] - u[, 3] * v[, 2],
                 u[, 3] * v[, 1] - u[, 1] * v[, 3],
                 u[, 1] * v[, 2] - u[, 2] * v[, 1])
  if (any(rowSums(abs(cross)) == 0)) {
    stop("Zero-area faces are not supported.", call. = FALSE)
  }
  all <- rbind(faces[, c(1, 2), drop = FALSE],
               faces[, c(2, 3), drop = FALSE],
               faces[, c(3, 1), drop = FALSE])
  edges <- cbind(pmin(all[, 1], all[, 2]), pmax(all[, 1], all[, 2]))
  key <- paste(edges[, 1], edges[, 2], sep = "/")
  ord <- order(edges[, 1], edges[, 2])
  keep <- ord[!duplicated(key[ord])]
  face_edges <- matrix(match(key, key[keep]), ncol = 3L)
  if (any(tabulate(face_edges, nbins = length(keep)) > 2L)) {
    stop("Edges with more than two incident faces are not supported.",
         call. = FALSE)
  }
  list(faces = faces, edges = edges[keep, , drop = FALSE],
       face_edges = face_edges)
}

.ns_partition_membership <- function(labels, edges, iterations) {
  n <- length(labels)
  ids <- sort(unique(labels))
  column <- match(labels, ids)
  m <- Matrix::sparseMatrix(i = seq_len(n), j = column, x = 1,
                            dims = c(n, length(ids)))
  if (iterations > 0L) {
    adj <- Matrix::sparseMatrix(i = c(edges[, 1], edges[, 2]),
                                j = c(edges[, 2], edges[, 1]), x = 1,
                                dims = c(n, n))
    degree <- Matrix::rowSums(adj)
    isolated <- which(degree == 0)
    # Isolated vertices keep their one-hot membership.
    if (length(isolated)) adj[cbind(isolated, isolated)] <- 1
    op <- Matrix::Diagonal(x = 1 / pmax(1, degree)) %*% adj
    for (it in seq_len(iterations)) m <- 0.5 * m + 0.5 * (op %*% m)
  }
  list(scores = m, column = column)
}

.ns_partition_crossing <- function(d_i, d_j, clamp, tau) {
  u <- pmax(d_i, 0)
  v <- pmax(-d_j, 0)
  fallback <- u + v <= tau
  raw <- rep(0.5, length(u))
  raw[!fallback] <- u[!fallback] / (u[!fallback] + v[!fallback])
  t <- pmin(clamp[2], pmax(clamp[1], raw))
  list(t_raw = raw, t = t, midpoint_fallback = fallback, clamped = t != raw)
}

.ns_project_partition_junction <- function(w, lo) {
  mass <- 1 - 3 * lo
  if (mass == 0) return(rep(lo, 3L))
  # Translation leaves a simplex projection unchanged. Removing the maximum
  # first avoids catastrophic cancellation for exterior junctions far away.
  z <- w - max(w)
  ordered <- sort(z, decreasing = TRUE)
  theta <- (cumsum(ordered) - mass) / seq_along(ordered)
  rho <- max(which(ordered > theta))
  lo + pmax(z - theta[rho], 0)
}

.ns_partition_junction <- function(scores, lo, tau) {
  h <- diag(3) - matrix(1/3, 3L, 3L)
  a <- rbind(h %*% t(scores), c(1, 1, 1))
  dec <- svd(a)
  fallback <- min(dec$d) <= tau * max(dec$d)
  raw <- if (fallback) rep(1/3, 3L) else {
    as.numeric(dec$v %*% (dec$u[4, ] / dec$d))
  }
  if (any(!is.finite(raw))) {
    raw <- rep(1/3, 3L)
    fallback <- TRUE
  }
  w <- .ns_project_partition_junction(raw, lo)
  list(raw = raw, w = w, centroid_fallback = fallback,
       projected = any(abs(w - raw) > tau))
}

.ns_chain_partition <- function(segments, nodes) {
  paths <- list()
  pairs <- matrix(integer(), 0L, 2L)
  if (!nrow(segments)) {
    return(list(boundary = list(), label_pair = pairs, closed = logical(),
                path_nodes = paths))
  }
  groups <- split(seq_len(nrow(segments)),
                  paste(segments[, 3], segments[, 4], sep = "/"))
  pair_list <- list()
  for (group in groups) {
    ends <- segments[group, 1:2, drop = FALSE]
    adj <- split(rep(seq_len(nrow(ends)), 2L), as.vector(ends))
    degree <- lengths(adj)
    if (any(degree > 2L)) {
      stop("Internal partition error: branching label-pair chain.",
           call. = FALSE)
    }
    used <- rep(FALSE, nrow(ends))
    starts <- c(sort(as.integer(names(adj)[degree == 1L])),
                sort(as.integer(names(adj)[degree == 2L])))
    for (start in starts) {
      if (all(used[adj[[as.character(start)]]])) next
      path <- integer(nrow(ends) + 1L)
      size <- 1L
      path[size] <- current <- start
      repeat {
        incident <- adj[[as.character(current)]]
        available <- incident[!used[incident]]
        if (!length(available)) break
        other <- ifelse(ends[available, 1] == current,
                         ends[available, 2], ends[available, 1])
        edge <- available[which.min(other)]
        current <- if (ends[edge, 1] == current) ends[edge, 2] else ends[edge, 1]
        used[edge] <- TRUE
        size <- size + 1L
        path[size] <- current
        if (current == start) break
      }
      path <- path[seq_len(size)]
      if (path[1] > path[size]) path <- rev(path)
      paths[[length(paths) + 1L]] <- path
      pair_list[[length(pair_list) + 1L]] <- segments[group[1], 3:4]
    }
  }
  pairs <- do.call(rbind, pair_list)
  ord <- order(pairs[, 1], pairs[, 2], vapply(paths, `[`, integer(1), 1L))
  paths <- paths[ord]
  xyz <- as.matrix(nodes[, c("x", "y", "z")])
  list(boundary = lapply(paths, function(p) unname(xyz[p, , drop = FALSE])),
       label_pair = unname(pairs[ord, , drop = FALSE]),
       closed = vapply(paths, function(p) p[1] == p[length(p)], logical(1)),
       path_nodes = paths)
}

.ns_build_parcel_partition <- function(vertices, faces, labels,
                                       boundary_smooth = 3L,
                                       t_clamp = c(0.1, 0.9)) {
  tau <- 1e-12
  vertices <- as.matrix(vertices)
  faces <- as.matrix(faces)
  mesh <- .ns_partition_mesh(vertices, faces)
  faces <- mesh$faces
  labels <- .ns_partition_labels(labels, nrow(vertices))
  iterations <- .ns_partition_iterations(boundary_smooth)
  clamp <- .ns_partition_clamp(t_clamp, tau)
  edges <- mesh$edges
  m <- .ns_partition_membership(labels, edges, iterations)
  scores <- m$scores
  column <- m$column
  crossing_edges <- which(labels[edges[, 1]] != labels[edges[, 2]])
  edge <- edges[crossing_edges, , drop = FALSE]
  i <- edge[, 1]
  j <- edge[, 2]
  di <- as.numeric(scores[cbind(i, column[i])] - scores[cbind(i, column[j])])
  dj <- as.numeric(scores[cbind(j, column[i])] - scores[cbind(j, column[j])])
  crossing <- .ns_partition_crossing(di, dj, clamp, tau)
  nc <- length(i)
  crossings <- data.frame(i = i, j = j, label_i = labels[i],
                          label_j = labels[j], node_id = seq_len(nc),
                          d_i = di, d_j = dj, t_raw = crossing$t_raw,
                          t = crossing$t,
                          midpoint_fallback = crossing$midpoint_fallback,
                          clamped = crossing$clamped)
  xyz <- (1 - crossing$t) * vertices[i, , drop = FALSE] +
    crossing$t * vertices[j, , drop = FALSE]
  nodes <- data.frame(id = seq_len(nc), kind = rep("crossing", nc),
                      x = xyz[, 1], y = xyz[, 2], z = xyz[, 3])
  edge_node <- integer(nrow(edges))
  edge_node[crossing_edges] <- seq_len(nc)
  face_nodes <- matrix(edge_node[mesh$face_edges], ncol = 3L)
  face_labels <- matrix(labels[faces], ncol = 3L)
  mixed <- face_labels[, 1] != face_labels[, 2] |
    face_labels[, 2] != face_labels[, 3]
  three <- which(face_labels[, 1] != face_labels[, 2] &
                   face_labels[, 2] != face_labels[, 3] &
                   face_labels[, 3] != face_labels[, 1])
  junctions <- data.frame(face_id = three, node_id = nc + seq_along(three),
                          w_raw1 = numeric(length(three)),
                          w_raw2 = numeric(length(three)),
                          w_raw3 = numeric(length(three)),
                          w1 = numeric(length(three)),
                          w2 = numeric(length(three)),
                          w3 = numeric(length(three)),
                          centroid_fallback = logical(length(three)),
                          projected = logical(length(three)))
  for (k in seq_along(three)) {
    fi <- three[k]
    s <- as.matrix(scores[faces[fi, ], column[faces[fi, ]], drop = FALSE])
    junction <- .ns_partition_junction(s, clamp[1], tau)
    junctions[k, 3:5] <- junction$raw
    junctions[k, 6:8] <- junction$w
    junctions$centroid_fallback[k] <- junction$centroid_fallback
    junctions$projected[k] <- junction$projected
  }
  if (length(three)) {
    w <- as.matrix(junctions[, 6:8])
    xyz <- w[, 1] * vertices[faces[three, 1], , drop = FALSE] +
      w[, 2] * vertices[faces[three, 2], , drop = FALSE] +
      w[, 3] * vertices[faces[three, 3], , drop = FALSE]
    nodes <- rbind(nodes, data.frame(id = junctions$node_id, kind = "junction",
                                    x = xyz[, 1], y = xyz[, 2], z = xyz[, 3]))
  }
  junction_index <- integer(nrow(faces))
  junction_index[three] <- seq_along(three)
  # Uniform faces have an implicit single cell; only boundary faces allocate
  # polygon lists. This keeps the prepared object compact on dense surfaces.
  cells <- vector("list", nrow(faces))
  segments <- matrix(0L, sum(mixed) + 2L * length(three), 4L)
  segment <- 0L
  basis <- diag(3)
  for (fi in which(mixed)) {
    ids <- face_nodes[fi, ]
    labs <- face_labels[fi, ]
    q <- matrix(0, 3L, 3L)
    for (corner in which(ids > 0L)) {
      next_corner <- corner %% 3L + 1L
      t <- crossings$t[ids[corner]]
      if (faces[fi, corner] != crossings$i[ids[corner]]) t <- 1 - t
      q[corner, corner] <- 1 - t
      q[corner, next_corner] <- t
    }
    ji <- junction_index[fi]
    if (ji == 0L) {
      odd <- which(!duplicated(labs) & !duplicated(labs, fromLast = TRUE))
      after <- odd %% 3L + 1L
      before <- (odd + 1L) %% 3L + 1L
      cells[[fi]] <- list(
        list(label = labs[odd], bary = rbind(basis[odd, ], q[odd, ], q[before, ])),
        list(label = labs[after],
             bary = rbind(q[odd, ], basis[after, ], basis[before, ], q[before, ]))
      )
      segment <- segment + 1L
      segments[segment, ] <- c(ids[odd], ids[before], sort(unique(labs)))
    } else {
      w <- as.numeric(junctions[ji, 6:8])
      cells[[fi]] <- lapply(1:3, function(corner) {
        before <- (corner + 1L) %% 3L + 1L
        list(label = labs[corner],
             bary = rbind(basis[corner, ], q[corner, ], w, q[before, ]))
      })
      for (corner in 1:3) {
        segment <- segment + 1L
        pair <- sort(labs[c(corner, corner %% 3L + 1L)])
        segments[segment, ] <- c(ids[corner], junctions$node_id[ji], pair)
      }
    }
  }
  provenance <- list(algorithm = "constrained_parcel_partition_v1",
                     kernel = "unique_one_ring_mean", lambda = 0.5,
                     boundary_smooth = iterations, t_clamp = clamp,
                     tolerance = tau, tie_rule = "smallest_incident_label")
  boundaries <- c(.ns_chain_partition(segments, nodes),
                  list(boundary_roi_id = NULL, roi_components = NULL,
                       boundary_verts = NULL, nodes = nodes,
                       crossings = crossings, junctions = junctions,
                       provenance = provenance))
  list(boundaries = boundaries, cells = cells, face_labels = face_labels,
       mixed = mixed, segments = segments, labels = labels,
       provenance = provenance)
}

.ns_partition_face_cells <- function(partition, face) {
  if (partition$mixed[face]) return(partition$cells[[face]])
  list(list(label = partition$face_labels[face, 1], bary = diag(3)))
}

.ns_partition_triangle_contains <- function(w, triangle) {
  inside <- rep(TRUE, nrow(w))
  for (i in 1:3) {
    j <- i %% 3L + 1L
    dx <- triangle[j, 2] - triangle[i, 2]
    dy <- triangle[j, 3] - triangle[i, 3]
    a <- dx * (w[, 3] - triangle[i, 3])
    b <- dy * (w[, 2] - triangle[i, 2])
    # Include the input-coordinate error: raster barycentrics reconstruct the
    # third weight by subtraction. At a crossing, the determinant's products
    # alone can vanish and underestimate that error. Scale by edge length so
    # the bound remains useful for small cells as well as ordinary ones.
    error <- 8 * .Machine$double.eps *
      (abs(a) + abs(b) + abs(dx) + abs(dy))
    inside <- inside & a - b >= -error
  }
  inside
}

.ns_classify_parcel_partition <- function(partition, face, weights) {
  out <- partition$face_labels[face, 1]
  indices <- which(partition$mixed[face])
  groups <- split(indices, face[indices])
  for (group in groups) {
    fi <- face[group[1]]
    w <- weights[group, , drop = FALSE]
    value <- rep(NA_integer_, nrow(w))
    for (cell in partition$cells[[fi]]) {
      p <- cell$bary
      inside <- rep(FALSE, nrow(w))
      for (k in seq.int(2L, nrow(p) - 1L)) {
        inside <- inside | .ns_partition_triangle_contains(
          w, p[c(1L, k, k + 1L), , drop = FALSE])
      }
      take <- inside & (is.na(value) | cell$label < value)
      value[take] <- cell$label
    }
    # Preserve exact original vertices even for clamps near machine precision.
    for (corner in 1:3) {
      at_vertex <- w[, corner] == 1 & rowSums(abs(w[, -corner, drop = FALSE])) == 0
      value[at_vertex] <- partition$face_labels[fi, corner]
    }
    if (anyNA(value)) {
      stop("Internal partition error: sample outside all face cells.",
           call. = FALSE)
    }
    out[group] <- value
  }
  as.integer(out)
}
