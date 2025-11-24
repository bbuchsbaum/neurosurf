test_that("roi_boundary_loops_cpp finds single hex ring", {
  center <- c(0, 0, 0)
  angles <- seq(0, 2 * pi - 2 * pi / 6, length.out = 6)
  outer <- cbind(cos(angles), sin(angles), rep(0, 6))

  vertices <- rbind(center, outer)
  faces <- cbind(
    rep(1L, 6L),
    2:7,
    c(3:7, 2L)
  )
  roi <- c(1L, rep(2L, 6L))

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = TRUE
  )

  expect_length(res$boundary, 1L)
  expect_equal(res$boundary_roi_id, 2L)
  expect_equal(res$roi_components, c(0, 1))

  loop <- res$boundary_verts[[1]]
  expect_equal(loop[1], loop[length(loop)])
  expect_true(all(loop %in% 2:7))
  expect_equal(length(loop), 7L)
})


test_that("roi_boundary_loops_cpp returns empty for single ROI", {
  vertices <- matrix(c(0, 0, 0,
                       1, 0, 0,
                       0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1L, 2L, 3L), ncol = 3, byrow = TRUE)
  roi <- c(1L, 1L, 1L)

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = TRUE
  )

  expect_length(res$boundary, 0L)
  expect_length(res$boundary_verts, 0L)
  expect_equal(res$roi_components, 0)
})


test_that("roi_boundary_loops_cpp separates multiple rings", {
  angles <- seq(0, 2 * pi - 2 * pi / 6, length.out = 6)
  outer1 <- cbind(cos(angles), sin(angles), rep(0, 6))
  outer2 <- cbind(3 + cos(angles), sin(angles), rep(0, 6))

  vertices <- rbind(
    c(0, 0, 0), outer1,
    c(3, 0, 0), outer2
  )

  faces1 <- cbind(
    rep(1L, 6L),
    2:7,
    c(3:7, 2L)
  )
  faces2 <- cbind(
    rep(8L, 6L),
    9:14,
    c(10:14, 9L)
  )

  faces <- rbind(faces1, faces2)
  roi <- c(1L, rep(2L, 6L), 1L, rep(2L, 6L))

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = TRUE
  )

  expect_length(res$boundary, 2L)
  expect_equal(res$roi_components, c(0, 2))
  expect_true(all(res$boundary_roi_id == 2L))

  boundary_vertices <- sort(unique(unlist(res$boundary_verts)))
  expect_setequal(boundary_vertices, c(2:7, 9:14))
})


# helper: turn a loop (with closing vertex) into sorted undirected edge strings
loop_edges <- function(vloop) {
  edges <- cbind(vloop[-length(vloop)], vloop[-1])
  edges <- t(apply(edges, 1, function(e) sort(e)))
  ord <- order(edges[, 1], edges[, 2])
  edges[ord, , drop = FALSE]
}


test_that("cpp boundaries match expected cube split loops", {
  # cube split into bottom ROI 1 and top ROI 2
  vertices <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    1, 1, 0,
    0, 1, 0,
    0, 0, 1,
    1, 0, 1,
    1, 1, 1,
    0, 1, 1
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 3, 1, 3, 4,
    5, 6, 7, 5, 7, 8,
    1, 2, 6, 1, 6, 5,
    3, 4, 8, 3, 8, 7,
    1, 4, 8, 1, 8, 5,
    2, 3, 7, 2, 7, 6
  ), ncol = 3, byrow = TRUE)

  roi <- c(1, 1, 1, 1, 2, 2, 2, 2)

  res_cpp <- find_roi_boundaries(vertices, faces, roi,
                                 boundary_method = "edge_vertices",
                                 use_cpp = TRUE)

  expect_equal(length(res_cpp$boundary_verts), 2L)
  expect_setequal(res_cpp$boundary_roi_id, c(1L, 2L))

  expected_loops <- list(
    c(1L, 2L, 3L, 4L, 1L),
    c(5L, 6L, 7L, 8L, 5L)
  )

  cpp_edges <- lapply(res_cpp$boundary_verts, loop_edges)
  exp_edges <- lapply(expected_loops, loop_edges)

  cpp_edges_sorted <- cpp_edges[order(sapply(cpp_edges, function(e) paste(e[1, ], collapse = ",")))]
  exp_edges_sorted <- exp_edges[order(sapply(exp_edges, function(e) paste(e[1, ], collapse = ",")))]

  expect_equal(cpp_edges_sorted, exp_edges_sorted)
})


test_that("non-degree-2 components still return closed loops", {
  vertices <- matrix(c(
    0, 0, 0,  # 1
    1, 0, 0,  # 2
    0, 1, 0,  # 3
    1, 1, 0,  # 4
    0, 0, 1   # 5 (different ROI)
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 5,
    2, 3, 5,
    3, 1, 5,
    2, 4, 5,
    1, 4, 5
  ), ncol = 3, byrow = TRUE)

  roi <- c(1, 1, 1, 1, 2)

  res <- find_roi_boundaries(vertices, faces, roi,
                             boundary_method = "edge_vertices",
                             use_cpp = TRUE)

  expect_true(length(res$boundary_verts) >= 1)
  loop <- res$boundary_verts[[1]]
  expect_equal(loop[1], loop[length(loop)])
  # no immediate backtracking
  expect_false(any(loop[-1] == loop[-length(loop)]))
})
