pp_fun <- function(name) get(paste0(".ns_", name), asNamespace("neurosurf"))

pp_tetra <- function() {
  list(v = rbind(c(1, 1, 1), c(1, -1, -1), c(-1, 1, -1), c(-1, -1, 1)),
       f = rbind(c(1L, 2L, 3L), c(1L, 4L, 2L),
                 c(1L, 3L, 4L), c(2L, 4L, 3L)))
}

pp_octa <- function() {
  list(v = rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0),
                 c(0, -1, 0), c(0, 0, 1), c(0, 0, -1)),
       f = rbind(c(1L, 3L, 5L), c(1L, 6L, 3L), c(1L, 5L, 4L),
                 c(1L, 4L, 6L), c(2L, 5L, 3L), c(2L, 3L, 6L),
                 c(2L, 4L, 5L), c(2L, 6L, 4L)))
}

pp_build <- function(mesh, labels, iterations = 3L, clamp = c(0.1, 0.9)) {
  pp_fun("build_parcel_partition")(mesh$v, mesh$f, labels, iterations, clamp)
}

pp_segments <- function(boundary) {
  segments <- do.call(rbind, lapply(boundary, function(p) {
    cbind(p[-nrow(p), , drop = FALSE], p[-1, , drop = FALSE])
  }))
  if (is.null(segments)) return(character())
  sort(apply(round(segments, 10), 1, function(s) {
    paste(sort(c(paste(s[1:3], collapse = ","),
                 paste(s[4:6], collapse = ","))), collapse = "/")
  }))
}

# Independent point-in-polygon oracle: ray crossings on the original polygon,
# rather than the production classifier's triangular determinant predicates.
pp_polygon_contains <- function(w, polygon, tol = 1e-12) {
  p <- polygon[, 2:3, drop = FALSE]
  x <- w[2]; y <- w[3]
  odd <- FALSE
  for (i in seq_len(nrow(p))) {
    j <- i %% nrow(p) + 1L
    u <- p[j, ] - p[i, ]
    relative <- c(x, y) - p[i, ]
    t <- sum(relative * u) / sum(u * u)
    if (t >= -tol && t <= 1 + tol &&
        sqrt(sum((relative - t * u)^2)) <= tol) return(TRUE)
    if ((p[i, 2] > y) != (p[j, 2] > y)) {
      intercept <- p[i, 1] + (y - p[i, 2]) * u[1] / u[2]
      if (x < intercept) odd <- !odd
    }
  }
  odd
}

pp_oracle <- function(partition, face, w) {
  vapply(seq_along(face), function(i) {
    fi <- face[i]
    cells <- partition$cells[[fi]]
    if (is.null(cells)) return(partition$face_labels[fi, 1])
    hit <- vapply(cells, function(cell) {
      pp_polygon_contains(w[i, ], cell$bary)
    }, logical(1))
    if (!any(hit)) stop("Oracle found an uncovered point")
    min(vapply(cells[hit], `[[`, integer(1), "label"))
  }, integer(1))
}

pp_polygon_area <- function(p) {
  next_row <- c(seq.int(2L, nrow(p)), 1L)
  sum(p[, 2] * p[next_row, 3] - p[next_row, 2] * p[, 3]) / 2
}

test_that("zero smoothing reproduces midpoint geometry after chaining", {
  mesh <- pp_tetra()
  for (labs in list(c(1L, 1L, 1L, 1L), c(2L, 1L, 1L, 1L),
                    c(1L, 2L, 3L, 3L))) {
    midpoint <- find_roi_boundaries(mesh$v, mesh$f, labs)
    smooth <- find_roi_boundaries(mesh$v, mesh$f, labs, "smooth",
                                   boundary_smooth = 0L)
    expect_equal(pp_segments(smooth$boundary), pp_segments(midpoint$boundary))
    expect_true(all(smooth$crossings$t == 0.5))
  }
})

test_that("endpoint margins implement bracket, fallback, and clamp rules", {
  f <- pp_fun("partition_crossing")
  di <- c(0.8, -0.4, 0.4, -0.4, 0, 1e-15)
  dj <- c(-0.2, -0.5, 0.5, 0.5, 0, -1e-15)
  x <- f(di, dj, c(0.1, 0.9), 1e-12)
  expect_equal(x$t_raw, c(0.8, 0, 1, 0.5, 0.5, 0.5))
  expect_equal(x$t, c(0.8, 0.1, 0.9, 0.5, 0.5, 0.5))
  expect_identical(x$midpoint_fallback, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(x$clamped, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(f(-dj, -di, c(0.1, 0.9), 1e-12)$t, 1 - x$t)
})

test_that("junction projection agrees with independent active-set solutions", {
  project <- pp_fun("project_partition_junction")
  # Exhaust all seven nonempty active sets of the three-variable projection.
  oracle <- function(w, lo) {
    candidate <- lapply(1:7, function(bits) {
      free <- which(as.logical(intToBits(bits)[1:3]))
      out <- rep(lo, 3L)
      mass <- 1 - (3 - length(free)) * lo
      out[free] <- w[free] - (sum(w[free]) - mass) / length(free)
      out
    })
    candidate <- Filter(function(x) all(x >= lo - 1e-13), candidate)
    loss <- vapply(candidate, function(x) sum((x - w)^2), numeric(1))
    candidate[[which.min(loss)]]
  }
  for (w in list(c(0.2, 0.3, 0.5), c(2.5, 2.5, -4), c(8, -1, -6),
                  c(-0.1, 0.4, 0.7), c(0, 0, 0))) {
    for (lo in c(0.01, 0.1, 1/3)) {
      expect_equal(project(w, lo), oracle(w, lo), tolerance = 1e-12)
    }
  }
  expect_equal(project(c(1e12, 1e12, -2e12), 0.1), c(0.45, 0.45, 0.1))
  junction <- pp_fun("partition_junction")
  x <- junction(diag(3), 0.1, 1e-12)
  expect_equal(x$w, rep(1/3, 3))
  expect_false(x$centroid_fallback)
  expect_false(x$projected)
  tied <- junction(matrix(1/3, 3, 3), 0.1, 1e-12)
  expect_true(tied$centroid_fallback)
  expect_equal(tied$w, rep(1/3, 3))
})

test_that("tetrahedron scores have the independent lazy-walk solution", {
  mesh <- pp_tetra()
  edges <- t(utils::combn(1:4, 2))
  m <- pp_fun("partition_membership")(c(1L, 2L, 3L, 4L), edges, 3L)$scores
  oracle <- matrix(1/4, 4L, 4L) + (diag(4L) - 1/4) / 27
  expect_equal(as.matrix(m), oracle, tolerance = 1e-14)
  isolated <- pp_fun("partition_membership")(c(1L, 2L, 3L, 4L, 5L),
                                               edges, 3L)$scores
  expect_equal(as.numeric(isolated[5, ]), c(0, 0, 0, 0, 1))
  p <- pp_build(mesh, c(1L, 2L, 2L, 2L))
  expect_equal(p$boundaries$crossings$t, rep(0.1, 3))
  p <- pp_build(mesh, c(1L, 2L, 3L, 3L))
  expect_equal(as.numeric(p$boundaries$junctions[1, 3:5]), c(2.5, 2.5, -4))
  expect_equal(as.numeric(p$boundaries$junctions[1, 6:8]), c(0.45, 0.45, 0.1))
})

test_that("face cells preserve vertices and form a complete partition", {
  grid <- expand.grid(b = seq(0, 1, length.out = 13),
                       c = seq(0, 1, length.out = 13))
  grid <- grid[grid$b + grid$c <= 1, ]
  w <- cbind(1 - grid$b - grid$c, grid$b, grid$c)
  cases <- list(list(mesh = pp_tetra(), labels = c(0L, 2L, 2L, 2L)),
                list(mesh = pp_tetra(), labels = c(1L, 2L, 3L, 3L)),
                list(mesh = pp_octa(), labels = c(1L, 3L, 2L, 3L, 3L, 4L)))
  for (case in cases) {
    p <- pp_build(case$mesh, case$labels)
    for (fi in seq_len(nrow(case$mesh$f))) {
      cells <- pp_fun("partition_face_cells")(p, fi)
      areas <- vapply(cells, function(x) pp_polygon_area(x$bary), numeric(1))
      expect_true(all(areas > 0))
      expect_equal(sum(areas), 0.5, tolerance = 1e-13)
      expect_identical(pp_fun("classify_parcel_partition")(p, rep(fi, 3), diag(3)),
                       case$labels[case$mesh$f[fi, ]])
      # Include every edge crossing and junction, not just pixel interiors.
      samples <- rbind(w, do.call(rbind, lapply(cells, `[[`, "bary")))
      ids <- rep(fi, nrow(samples))
      expect_identical(pp_fun("classify_parcel_partition")(p, ids, samples),
                       pp_oracle(p, ids, samples))
      # A point strictly inside each cell belongs to exactly one polygon.
      for (cell in cells) {
        # The first fan triangle has positive area even for concave cells.
        centre <- colMeans(cell$bary[1:3, , drop = FALSE])
        expect_equal(sum(vapply(cells, function(x) {
          pp_polygon_contains(centre, x$bary)
        }, logical(1))), 1L)
      }
    }
    b <- p$boundaries
    q <- (1 - b$crossings$t) * case$mesh$v[b$crossings$i, , drop = FALSE] +
      b$crossings$t * case$mesh$v[b$crossings$j, , drop = FALSE]
    expect_equal(unname(as.matrix(b$nodes[b$crossings$node_id, 3:5])),
                 unname(q), tolerance = 1e-13)
  }
})

test_that("incident faces agree along the octahedron's candidate seam", {
  p <- pp_build(pp_octa(), c(1L, 3L, 2L, 3L, 3L, 4L))
  q <- p$boundaries$crossings
  t <- q$t[q$i == 1L & q$j == 3L]
  fractions <- sort(unique(c(0, 0.1, 0.4, 0.5, 0.9, 1, t)))
  w1 <- cbind(1 - fractions, fractions, 0)
  w2 <- cbind(1 - fractions, 0, fractions)
  classify <- pp_fun("classify_parcel_partition")
  expect_identical(classify(p, rep(1L, length(fractions)), w1),
                   classify(p, rep(2L, length(fractions)), w2))
})

test_that("thin necks and skew triangles retain every original vertex", {
  grid <- expand.grid(x = -3:3, y = -2:2)
  vertex <- function(x, y) (y + 2L) * 7L + x + 4L
  faces <- list()
  for (y in -2:1) for (x in -3:2) {
    a <- vertex(x, y); b <- vertex(x + 1L, y)
    c <- vertex(x + 1L, y + 1L); d <- vertex(x, y + 1L)
    faces <- c(faces, list(c(a, b, c), c(a, c, d)))
  }
  mesh <- list(v = cbind(as.matrix(grid), 0), f = do.call(rbind, faces))
  labels <- as.integer(grid$y == 0 | (abs(grid$x) >= 2 & abs(grid$y) <= 1))
  p <- pp_build(mesh, labels)
  for (fi in seq_len(nrow(mesh$f))) {
    expect_identical(pp_fun("classify_parcel_partition")(p, rep(fi, 3), diag(3)),
                     labels[mesh$f[fi, ]])
  }
  skew <- list(v = rbind(c(0, 0, 0), c(10, 0, 0), c(0.01, 0.001, 0)),
                 f = matrix(1:3, 1, 3))
  for (lo in c(0.1, 1/3, 1e-15)) {
    p <- pp_build(skew, c(3L, 2L, 1L), clamp = c(lo, 1 - lo))
    expect_identical(pp_fun("classify_parcel_partition")(p, rep(1L, 3), diag(3)),
                     c(3L, 2L, 1L))
    cells <- p$cells[[1]]
    expect_equal(sum(vapply(cells, function(x) pp_polygon_area(x$bary),
                            numeric(1))), 0.5)
  }
})

test_that("paths cover segments once with shared junctions and closed cycles", {
  mesh <- pp_tetra()
  for (labels in list(c(1L, 2L, 2L, 2L), c(1L, 2L, 3L, 3L))) {
    p <- pp_build(mesh, labels)
    b <- p$boundaries
    key <- function(a, z) paste(pmin(a, z), pmax(a, z), sep = "/")
    emitted <- unlist(lapply(b$path_nodes, function(path) {
      key(path[-length(path)], path[-1])
    }))
    expect_equal(sort(emitted), sort(key(p$segments[, 1], p$segments[, 2])))
    expect_false(anyDuplicated(emitted) > 0)
    for (k in seq_along(b$path_nodes)) {
      path <- b$path_nodes[[k]]
      expect_identical(path[1] == path[length(path)], b$closed[k])
      unique_part <- if (b$closed[k]) path[-length(path)] else path
      expect_equal(length(unique(unique_part)), length(unique_part))
      expect_true(b$label_pair[k, 1] < b$label_pair[k, 2])
    }
    for (node in b$junctions$node_id) {
      endpoints <- unlist(lapply(b$path_nodes, function(path) {
        path[c(1L, length(path))]
      }))
      expect_equal(sum(endpoints == node), 3L)
    }
  }
  expect_true(all(pp_build(mesh, c(1L, 2L, 2L, 2L))$boundaries$closed))
  open <- list(v = mesh$v, f = mesh$f[1, , drop = FALSE])
  expect_false(any(pp_build(open, c(1L, 2L, 2L, 2L))$boundaries$closed))
  # Coincident but disjoint topology must give two separate identical loops.
  twice <- list(v = rbind(mesh$v, mesh$v), f = rbind(mesh$f, mesh$f + 4L))
  b <- pp_build(twice, rep(c(1L, 2L, 2L, 2L), 2))$boundaries
  expect_length(b$boundary, 2L)
  expect_equal(b$boundary[[1]], b$boundary[[2]])
  expect_length(intersect(b$path_nodes[[1]], b$path_nodes[[2]]), 0L)
  expect_identical(b, pp_build(twice, rep(c(1L, 2L, 2L, 2L), 2))$boundaries)
})

test_that("smooth geometry is invariant under input order and coordinates", {
  mesh <- pp_octa()
  labels <- c(1L, 3L, 2L, 3L, 3L, 4L)
  ref <- pp_build(mesh, labels)$boundaries
  flipped <- mesh
  flipped$f <- mesh$f[rev(seq_len(nrow(mesh$f))), c(3, 2, 1)]
  expect_equal(pp_segments(pp_build(flipped, labels)$boundaries$boundary),
               pp_segments(ref$boundary))
  perm <- c(4L, 6L, 2L, 1L, 5L, 3L)
  renumbered <- list(v = mesh$v[perm, ],
                     f = matrix(match(mesh$f, perm), ncol = 3L))
  expect_equal(pp_segments(pp_build(renumbered, labels[perm])$boundaries$boundary),
               pp_segments(ref$boundary))
  expect_equal(pp_segments(pp_build(mesh, c(19L, 2L, 41L, 7L)[labels])$
                            boundaries$boundary), pp_segments(ref$boundary))
  rotation <- rbind(c(0, 1, 0), c(0, 0, 1), c(1, 0, 0))
  for (scale in c(1e-50, 2, 1e50)) {
    moved <- list(v = mesh$v %*% rotation * scale, f = mesh$f)
    paths <- pp_build(moved, labels)$boundaries$boundary
    paths <- lapply(paths, function(x) (x / scale) %*% t(rotation))
    expect_equal(pp_segments(paths), pp_segments(ref$boundary))
  }
  shifted <- list(v = sweep(mesh$v, 2, c(12, -8, 3), "+"), f = mesh$f)
  paths <- pp_build(shifted, labels)$boundaries$boundary
  paths <- lapply(paths, function(x) sweep(x, 2, c(12, -8, 3)))
  expect_equal(pp_segments(paths), pp_segments(ref$boundary))
})

test_that("smooth validation rejects malformed inputs without coercion", {
  mesh <- pp_tetra()
  labels <- c(1L, 2L, 3L, 3L)
  for (bad in list(c(1, 2, 3, NA), c(1, 2, 3, Inf), c(1, 2, 3, 1.5),
                   c(1, 2, 3, -1), c(1, 2, 3, 2^31), c(1, 2, 3))) {
    expect_error(pp_build(mesh, bad), "nonnegative integer")
  }
  for (bad in list(-1, NA_real_, Inf, 1.5, c(1, 2), "3", 2^31)) {
    expect_error(pp_build(mesh, labels, bad), "non-negative integer")
  }
  for (bad in list(NULL, c(0, 1), c(0.4, 0.6), c(0.1, 0.8),
                   c(NA, 0.9), c(0.1, Inf), c(1e-20, 1))) {
    expect_error(pp_build(mesh, labels, clamp = bad), "t_clamp")
  }
  bad <- mesh
  bad$f <- rbind(mesh$f, mesh$f[1, 3:1])
  expect_error(pp_build(bad, labels), "Duplicate faces")
  bad$f <- mesh$f; bad$f[1, 2] <- bad$f[1, 1]
  expect_error(pp_build(bad, labels), "Repeated vertex")
  bad$f <- mesh$f; bad$f[1, 1] <- 0
  expect_error(pp_build(bad, labels), "1-based integer")
  bad$f[1, 1] <- 1.5
  expect_error(pp_build(bad, labels), "1-based integer")
  bad <- mesh; bad$v[1, 1] <- Inf
  expect_error(pp_build(bad, labels), "finite n x 3")
  bad <- mesh; bad$v[2, ] <- bad$v[1, ]
  expect_error(pp_build(bad, labels), "Zero-area")
  bad <- list(v = rbind(mesh$v, c(2, 3, 5)),
               f = rbind(c(1L, 2L, 3L), c(1L, 2L, 4L), c(1L, 2L, 5L)))
  expect_error(pp_build(bad, c(labels, 1L)), "two incident faces")
})

test_that("empty boundaries and S4 wrappers retain the smooth schema", {
  mesh <- pp_tetra()
  b <- pp_build(mesh, rep(1L, 4))$boundaries
  expect_identical(b$label_pair, matrix(integer(), 0, 2))
  expect_identical(b$boundary, list())
  expect_identical(b$closed, logical())
  expect_null(b$boundary_roi_id)
  expect_null(b$roi_components)
  expect_null(b$boundary_verts)
  expect_equal(nrow(b$nodes), 0L)
  expect_equal(nrow(b$crossings), 0L)
  expect_equal(nrow(b$junctions), 0L)
  g <- SurfaceGeometry(mesh$v, mesh$f - 1L, "left")
  x <- NeuroSurface(g, indices = 1:4, data = rep(1L, 4))
  expect_identical(findBoundaries(x, "smooth"), b)
  x <- NeuroSurface(g, indices = 1:4, data = c(1, 2, 3, 3.5))
  expect_error(findBoundaries(x, "smooth"), "nonnegative integer")
  no_faces <- list(v = mesh$v, f = matrix(integer(), 0L, 3L))
  expect_length(pp_build(no_faces, 1:4)$boundaries$boundary, 0L)
})

test_that("smooth renderer agrees with independent polygon classification", {
  mesh <- pp_octa()
  labels <- c(1L, 3L, 2L, 3L, 3L, 4L)
  g <- SurfaceGeometry(mesh$v, mesh$f - 1L, "left")
  p <- prepare_surface_parcels(g, labels, boundary_method = "smooth")
  expect_identical(p$partition$boundaries,
                   find_roi_boundaries(mesh$v, mesh$f, labels, "smooth"))
  for (camera in c("lateral", "medial")) {
    for (ss in c(1L, 2L)) {
      size <- 24L
      projected <- pp_fun("project_surface_camera")(
        mesh$v, camera, "left", size * ss, size * ss, margin = 0.01,
        presentation_obliquity = 0)
      gb <- get("cpp_rasterize_surface_gbuffer", asNamespace("neurosurf"))(
        projected, mesh$f, size * ss, size * ss)
      covered <- gb$face > 0
      w <- cbind(gb$w0[covered], gb$w1[covered],
                  1 - gb$w0[covered] - gb$w1[covered])
      reference <- matrix(NA_integer_, size * ss, size * ss)
      reference[covered] <- pp_oracle(p$partition, gb$face[covered], w)
      centre <- ceiling(ss / 2)
      select <- seq(centre, by = ss, length.out = size)
      reference <- reference[select, select]
      result <- render_surface_parcels(p, width = size, height = size,
                                       antialias = ss, camera = camera,
                                       values = c(`1` = 1, `2` = 2))
      expect_identical(result$labels, reference)
      expect_identical(result$provenance$partition, p$partition$provenance)
      expect_identical(result$provenance$boundary_method, "smooth")
    }
  }
  original <- p$partition
  invisible(render_surface_parcels(p, width = 24L, height = 24L,
                                    colors = c(`1` = "red")))
  expect_identical(p$partition, original)
  masked <- prepare_surface_parcels(g, labels, cortex_mask = labels != 1,
                                     boundary_method = "smooth")
  expect_identical(masked$partition$labels, replace(labels, labels == 1, 0L))
  expect_error(prepare_surface_parcels(g, labels, boundary_smooth = 0.5),
                "non-negative integer")
  legacy <- prepare_surface_parcels(g, labels)
  explicit <- prepare_surface_parcels(g, labels, boundary_method = "membership")
  expect_identical(legacy, explicit)
  before_field <- legacy
  before_field$boundary_method <- NULL
  before_field$partition <- NULL
  args <- list(width = 24L, height = 24L)
  expect_identical(do.call(render_surface_parcels, c(list(before_field), args)),
                   do.call(render_surface_parcels, c(list(legacy), args)))
})

test_that("raster coverage tolerance cannot leave smooth samples unclassified", {
  v <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0))
  f <- matrix(1:3, 1, 3)
  g <- SurfaceGeometry(v, f - 1L, "left")
  p <- prepare_surface_parcels(g, 1:3, boundary_method = "smooth")
  margin <- 0.05 + 1e-11
  projected <- pp_fun("project_surface_camera")(
    v, "dorsal", "left", 10L, 10L, margin, presentation_obliquity = 0)
  gb <- get("cpp_rasterize_surface_gbuffer", asNamespace("neurosurf"))(
    projected, f, 10L, 10L)
  cov <- gb$face > 0
  w <- cbind(gb$w0[cov], gb$w1[cov], 1 - gb$w0[cov] - gb$w1[cov])
  expect_true(any(w < 0))
  # The coverage kernel admits points up to 1e-9 outside a face. The public
  # policy clips these into the face for labels, leaving depth/shading alone.
  w[w < 0] <- 0
  w <- w / rowSums(w)
  expected <- matrix(NA_integer_, 10L, 10L)
  expected[cov] <- pp_oracle(p$partition, gb$face[cov], w)
  result <- render_surface_parcels(p, camera = "dorsal", width = 10L,
                                   height = 10L, antialias = 1L, margin = margin)
  expect_identical(result$labels, expected)
  expect_identical(result$provenance$sample_edge_policy,
                   "clip_negative_barycentrics_and_renormalize")
})
