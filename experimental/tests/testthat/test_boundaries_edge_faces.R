library(testthat)
library(neurosurf)

test_that("edge_faces handles complex ROI boundaries", {
  vertices <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    1, 1, 0,
    0, 1, 0,
    0.5, 0.5, 0
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 5,
    2, 3, 5,
    3, 4, 5,
    4, 1, 5
  ), ncol = 3, byrow = TRUE)

  vertex_id <- c(1, 1, 1, 1, 2)

  res <- find_roi_boundaries(vertices, faces, vertex_id, boundary_method = "edge_faces")
  expect_true(length(res$boundary) >= 1)
  expect_true(all(vapply(res$boundary_verts, length, integer(1)) >= 4))
})

