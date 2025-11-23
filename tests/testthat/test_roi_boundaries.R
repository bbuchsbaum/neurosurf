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
